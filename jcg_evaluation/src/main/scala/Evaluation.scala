import java.io.*
import java.nio.file.*
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import play.api.libs.json.Json
import org.opalj.br.MethodDescriptor

import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt
import scala.util.Using
import scala.jdk.StreamConverters.*

object Evaluation {

    def main(args: Array[String]): Unit = {
        val config = ConfigParser.parseConfig(args).getOrElse {
            throw IllegalArgumentException(s"Cannot parse commandline arguments: ${args.mkString(" ")}")
        }

        val jreLocations = EvaluationHelper.getJRELocations(config.JRE_LOCATIONS_FILE)

        Files.createDirectories(config.outputDir)

        val projectSpecPaths =
            Files.list(config.inputDir)
                .filter(path => path.toString.endsWith(".conf") && path.toString.contains(config.projectFilter))
                .sorted
                .toScala(List)

        for {
            adapter <- config.adapters
            cgAlgo <- adapter.possibleAlgorithms.filter(_.toLowerCase().startsWith(config.algorithmFilter.toLowerCase()))
        } {
            val experimentOutputPath = config.outputDir.resolve(adapter.frameworkName, cgAlgo)
            Files.createDirectories(experimentOutputPath)

            Using(makeFingerprintWriter(experimentOutputPath, adapter, cgAlgo)) { fingerprintWriter =>

                for (projectSpecPath <- projectSpecPaths) {

                    val projectSpec = Json.parse(new FileInputStream(projectSpecPath.toFile)).validate[ProjectSpecification].get
                    val testCase = projectSpecPath.getFileName.toString.stripSuffix(".conf")

                    val callGraphPath = experimentOutputPath.resolve(s"$testCase.json.gz")

                    if (!config.skipAnalysis)
                        runAnalysis(config, jreLocations, adapter, cgAlgo, projectSpec, testCase, experimentOutputPath, callGraphPath, fingerprintWriter)

                    assessCallGraph(config, jreLocations, projectSpec, testCase, experimentOutputPath, callGraphPath, fingerprintWriter)
                }
            }
        }
    }

    private def runAnalysis(
        config: JCGConfig,
        jreLocations: Map[Int, String],
        adapter: TestAdapter,
        cgAlgo: String,
        projectSpec: ProjectSpecification,
        testCase: String,
        experimentOutputPath: Path,
        callGraphPath: Path,
        fingerprintWriter: PrintWriter
    ) = {
        Using(makeCallGraphWriter(callGraphPath)) { callGraphWriter =>

            println(s"running ${adapter.frameworkName} $cgAlgo against ${projectSpec.name}")

            val future = Future {
                try {
                    adapter.serializeCG(
                        cgAlgo,
                        projectSpec.target(config.inputDir.toFile).getCanonicalPath,
                        callGraphWriter,
                        AdapterOptions.makeJavaOptions(
                            projectSpec.main.orNull,
                            projectSpec.allClassPathEntryPaths(config.inputDir.toFile),
                            jreLocations(projectSpec.java),
                            analyzeJDK = false,
                            programArgs = config.programArgs.split(" ")
                        )
                    )
                } catch {
                    case e: Throwable =>
                        println(s"exception in project ${projectSpec.name}")
                        if (config.debug) {
                            e.printStackTrace()
                        }
                        -1
                }
            }


            try {
                val elapsed = tryAwait(config.timeout, future)
                reportTiming(experimentOutputPath, testCase, elapsed)
            } catch {
                case _: TimeoutException =>
                    println(s"Timeout after ${config.timeout} seconds")
                    val result = Timeout
                    fingerprintWriter.println(s"$testCase\t${result.shortNotation}")
                    reportTiming(experimentOutputPath, testCase, -1)
                case e: Throwable => println(e.getMessage)
            } finally {
                System.gc()
            }
        }
    }

    private def assessCallGraph(config: JCGConfig, jreLocations: Map[Int, String], projectSpec: ProjectSpecification, testCase: String, experimentOutputPath: Path, callGraphPath: Path, fingerprintWriter: PrintWriter): Unit = {
        val assessment = config.language match {
            case "java" =>
                val callGraph = Using(GZIPInputStream(BufferedInputStream(FileInputStream(callGraphPath.toFile)))) { stream =>
                    Json.parse(stream).validate[ReachableMethods].get.toMap
                }.get

                CGMatcher.matchCallSites(
                    projectSpec,
                    jreLocations(projectSpec.java),
                    experimentOutputPath.toFile,
                    callGraph,
                    config.debug
                )
            case "javascript" | "python" =>

                if(! Files.exists(callGraphPath))
                    throw IllegalArgumentException(s"Call graph file $callGraphPath does not exist.")

                val callGraph = new AdapterCG(callGraphPath.toFile)

                val expectedCallGraphPath = config.inputDir.resolve(s"$testCase.json")

                if(! Files.exists(expectedCallGraphPath))
                    throw IllegalArgumentException(s"Call graph file $expectedCallGraphPath does not exist.")

                val expectedCG = new ExpectedCG(expectedCallGraphPath.toFile)

                val isSound = callGraph.compareLinks(expectedCG).length == 0
                if (isSound) Sound else Unsound
        }

        fingerprintWriter.write(s"$testCase -> $assessment\n")

    }

    private def reportTiming(experimentOutputPath: Path, testCase: String, elapsed: Long): Unit = {
        val seconds = elapsed / 1000000000d
        val pw = new PrintWriter(experimentOutputPath.resolve(s"${testCase}-timings.txt").toFile)
        pw.write(s"$seconds sec.")
        pw.close()
        println(s"analysis took $seconds sec.")
    }


    /**
     * Expects future that generates call graph, awaits it for a given timeout
     * and on timeout writes the timeout to the fingerprint and evaluation file.
     *
     * @param timeout           The timeout in seconds.
     * @param future            The future that generates the call graph.
     */
    @throws[TimeoutException](classOf[TimeoutException])
    @throws[InterruptedException](classOf[InterruptedException])
    protected def tryAwait(
        timeout: Int,
        future: Future[Long]
    ): Long = {
        val duration =
            if (timeout >= 0)
                timeout.seconds
            else Duration.Inf
        Await.result(future, duration)
    }

    /**
     * Creates a PrintWriter for the fingerprint file.
     * @param experimentOutputPath The directory where the fingerprint file should be created.
     * @param adapter The test adapter used to create fingerprints.
     * @param cgAlgorithm The call graph algorithm used to create the fingerprints.
     * @return
     */
    protected def makeFingerprintWriter(experimentOutputPath: Path, adapter: TestAdapter, cgAlgorithm: String): PrintWriter = {
        val fingerprintPath = experimentOutputPath.resolve(s"${adapter.frameworkName}-$cgAlgorithm.profile")
        Files.deleteIfExists(fingerprintPath)
        PrintWriter(fingerprintPath.toFile)
    }

    protected def makeCallGraphWriter(callGraphPath: Path): Writer = {
        Files.deleteIfExists(callGraphPath)
        OutputStreamWriter(new GZIPOutputStream(BufferedOutputStream(FileOutputStream(callGraphPath.toFile))))
    }
}
