import java.io.*
import java.nio.file.*
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import play.api.libs.json.Json
import org.opalj.br.MethodDescriptor

import java.nio.charset.StandardCharsets
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

object Commandline {

    def main(args: Array[String]): Unit = {
        val options = CommandlineParser.parseConfig(args)

        val jreLocations = Util.getJRELocations(options)

        Files.createDirectories(options.callGraphsDir)

        val projectSpecPaths =
            Files.list(options.projectsDir)
                .filter(path => path.toString.endsWith(".conf") && path.toString.contains(options.projectFilter))
                .sorted
                .toScala(List)

        for {
            adapter <- options.adapters
            cgAlgo <- adapter.possibleAlgorithms.filter(_.toLowerCase().startsWith(options.algorithmFilter.toLowerCase()))
        } {
            val callGraphsDirectory = options.callGraphsDir.resolve(adapter.frameworkName, cgAlgo)
            Files.createDirectories(callGraphsDirectory)

            for (projectSpecPath <- projectSpecPaths) {

                val projectSpec = Json.parse(new FileInputStream(projectSpecPath.toFile)).validate[ProjectSpecification].get
                val testCase = projectSpecPath.getFileName.toString.stripSuffix(".conf")

                options.action match
                    case Action.Analyze => runAnalysis(options, jreLocations, projectSpec, adapter, cgAlgo, callGraphsDirectory, testCase)
                    case Action.Assess => assessCallGraph(options, jreLocations, projectSpec, callGraphsDirectory, testCase)
                    case Action.Size => computeCallGraphSize(options, callGraphsDirectory, testCase)
                    case Action.PrecisionRecall => computePrecisionRecall(options, callGraphsDirectory, testCase)
            }

        }
    }

    private def runAnalysis(options: CommandlineOptions, jreLocations: Map[Int, Path], projectSpec: ProjectSpecification, adapter: TestAdapter, cgAlgo: String, callGraphsDirectory: Path, testCase: String): Any = {
        val callGraphPath =
            if(options.compress)
                callGraphsDirectory.resolve(s"$testCase-callgraph.json.gz")
            else
                callGraphsDirectory.resolve(s"$testCase-callgraph.gz")

        Using(makeCallGraphWriter(callGraphPath)) { callGraphWriter =>

            println(s"running ${adapter.frameworkName} $cgAlgo against ${projectSpec.name}")

            val future = Future {
                try {
                    adapter.serializeCG(
                        cgAlgo,
                        projectSpec.target(options.projectsDir.toFile).getCanonicalPath,
                        callGraphWriter,
                        AdapterOptions.makeJavaOptions(
                            projectSpec.main.orNull,
                            projectSpec.allClassPathEntryPaths(options.projectsDir.toFile),
                            jreLocations(projectSpec.java),
                            target = projectSpec.target(options.projectsDir.toFile).toString,
                            analyzeJDK = options.analyzeJdk,
                            analysisArguments = options.analysisArgs.split(" ")
                        )
                    )
                } catch {
                    case e: Throwable =>
                        println(s"exception in project ${projectSpec.name}")
                        e.printStackTrace()
                        -1
                }
            }

            try {
                val elapsed = tryAwait(options.timeout, future)
                reportTiming(callGraphsDirectory, testCase, elapsed)
            } catch {
                case _: TimeoutException =>
                    println(s"Timeout after ${options.timeout} seconds")
                    val result = Timeout
                    reportTiming(callGraphsDirectory, testCase, -1)
                case e: Throwable => println(e.getMessage)
            } finally {
                System.gc()
            }
        }
    }

    private def assessCallGraph(options: CommandlineOptions, jreLocations: Map[Int, Path], projectSpec: ProjectSpecification, callGraphDirectory: Path, testCase: String): Unit = {
        val callGraphPath = Util.findCallGraphFile(callGraphDirectory, testCase)

        val assessment: Assessment = options.language match {
            case "java" =>
                val callGraph = Util.readReachableMethods(callGraphPath).toMap

                CGMatcher.matchCallSites(
                    projectSpec,
                    jreLocations(projectSpec.java),
                    callGraphDirectory.toFile,
                    callGraph,
                    options.debug
                )
            case "javascript" | "python" =>

                if (!Files.exists(callGraphPath))
                    throw IllegalArgumentException(s"Call graph file $callGraphPath does not exist.")

                val callGraph = new AdapterCG(callGraphPath.toFile)

                val expectedCallGraphPath = options.projectsDir.resolve(s"$testCase.json")

                if (!Files.exists(expectedCallGraphPath))
                    throw IllegalArgumentException(s"Call graph file $expectedCallGraphPath does not exist.")

                val expectedCG = new ExpectedCG(expectedCallGraphPath.toFile)

                val isSound = callGraph.compareLinks(expectedCG).length == 0
                if (isSound) Sound else Unsound
        }

        val outputPath = callGraphDirectory.resolve(s"$testCase-assessment.txt")

        Files.write(outputPath, assessment.toString.getBytes(StandardCharsets.UTF_8))
    }

    def computeCallGraphSize(options: CommandlineOptions, callGraphDirectory: Path, testCase: String): Unit = {

        val callGraphPath = Util.findCallGraphFile(callGraphDirectory, testCase)
        val reachableMethods = Util.readReachableMethods(callGraphPath).reachableMethods

        val appMethods = reachableMethods.count { rm =>
            val declClass = rm.method.declaringClass
            options.reachableMethodsInclude.matches(declClass)
        }

        val edgeCount = reachableMethods.foldLeft(0){ (acc, rm) =>
            acc + rm.callSites.foldLeft(0)((acc,cs) => acc + cs.targets.size)
        }

        val outputPath = callGraphDirectory.resolve(s"$testCase-size.json")

        Files.write(
            outputPath,
            Json.prettyPrint(Json.obj(
                "reachableMethods" -> reachableMethods.size,
                "edges" -> edgeCount
            )).getBytes(StandardCharsets.UTF_8)
        )
    }

    def computePrecisionRecall(options: CommandlineOptions, callGraphDirectory: Path, testCase: String): Unit = {
        try {
            val predictedCallGraphPath = Util.findCallGraphFile(callGraphDirectory, testCase)
            val predictedCallGraph = Util.readReachableMethods(predictedCallGraphPath).toMap

            val truthCallGraphsDirectory = options.truthCallGraphsDirectory.resolve("Dynamic", "Dynamic")
            val truthCallGraphPath = Util.findCallGraphFile(truthCallGraphsDirectory, testCase)
            val truthCallGraph = Util.readReachableMethods(truthCallGraphPath).toMap

            val precisionRecall = PrecisionRecall(
                actualCallGraph = truthCallGraph,
                predictedCallGraph = predictedCallGraph,
                reachableMethodsInclude = options.reachableMethodsInclude,
                edgeInclude = options.edgesInclude
            )

            val outputPath = callGraphDirectory.resolve(s"$testCase-${options.comparisonName}-precision-recall.json")
            Files.write(
                outputPath,
                Json.prettyPrint(
                    Json.obj(
                        "methods" ->
                            Json.obj(
                                "precision" -> precisionRecall.methods.precision,
                                "recall" -> precisionRecall.methods.recall,
                                "f1-score" -> precisionRecall.methods.f1Score,
                                "true_positive" -> precisionRecall.methods.truePositive.size,
                                "false_positive" -> precisionRecall.methods.falsePositive.size,
                                "false_negative" -> precisionRecall.methods.falseNegative.size,
                            ),
                        "edges" ->
                            Json.obj(
                                "precision" -> precisionRecall.edges.precision,
                                "recall" -> precisionRecall.edges.recall,
                                "f1-score" -> precisionRecall.edges.f1Score,
                                "true_positive" -> precisionRecall.edges.truePositive.size,
                                "false_positive" -> precisionRecall.edges.falsePositive.size,
                                "false_negative" -> precisionRecall.edges.falseNegative.size
                            ),
                        "edges-with-callsite-line-numbers" ->
                            Json.obj(
                                "precision" -> precisionRecall.edgesWithCallSiteLineNumbers.precision,
                                "recall" -> precisionRecall.edgesWithCallSiteLineNumbers.recall,
                                "f1-score" -> precisionRecall.edgesWithCallSiteLineNumbers.f1Score,
                                "true_positive" -> precisionRecall.edgesWithCallSiteLineNumbers.truePositive.size,
                                "false_positive" -> precisionRecall.edgesWithCallSiteLineNumbers.falsePositive.size,
                                "false_negative" -> precisionRecall.edgesWithCallSiteLineNumbers.falseNegative.size
                            )
                    )
                ).getBytes(StandardCharsets.UTF_8)
            )

            val classification = callGraphDirectory.resolve(s"$testCase-${options.comparisonName}-classification.json.gz")
            Using(GZIPOutputStream(BufferedOutputStream(FileOutputStream(classification.toFile)))) { writer =>
                writer.write(
                    Json.prettyPrint(
                        Json.obj(
                            "methods" ->
                                Json.obj(
                                    "true_positive" -> Json.toJson(precisionRecall.methods.truePositive),
                                    "false_positive" -> Json.toJson(precisionRecall.methods.falsePositive),
                                    "false_negative" -> Json.toJson(precisionRecall.methods.falseNegative)
                                ),
                            "edges" ->
                                Json.obj(
                                    "true_positive" -> Json.toJson(precisionRecall.edges.truePositive),
                                    "false_positive" -> Json.toJson(precisionRecall.edges.falsePositive),
                                    "false_negative" -> Json.toJson(precisionRecall.edges.falseNegative)
                                ),
                            "edges-with-callsite-line-numbers" ->
                                Json.obj(
                                    "true_positive" -> Json.toJson(precisionRecall.edgesWithCallSiteLineNumbers.truePositive),
                                    "false_positive" -> Json.toJson(precisionRecall.edgesWithCallSiteLineNumbers.falsePositive),
                                    "false_negative" -> Json.toJson(precisionRecall.edgesWithCallSiteLineNumbers.falseNegative)
                                )
                        )
                    ).getBytes(StandardCharsets.UTF_8)
                )
            }
        } catch {
            case exc: Throwable =>
                exc.printStackTrace()
        }

    }


    ///////////////////////////// Helper Functions //////////////////////////////////////

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
        if(callGraphPath.toString.endsWith(".gz")) {
            OutputStreamWriter(GZIPOutputStream(BufferedOutputStream(FileOutputStream(callGraphPath.toFile))))
        } else {
            OutputStreamWriter(BufferedOutputStream(FileOutputStream(callGraphPath.toFile)))
        }
    }
}
