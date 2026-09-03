import java.io.*
import java.nio.file.*
import java.util.zip.GZIPOutputStream
import play.api.libs.json.{JsValue, Json, Writes, __}

import java.nio.charset.StandardCharsets
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt
import scala.util.Using
import scala.jdk.StreamConverters.*
import scala.util.matching.compat.Regex

object Commandline {

    def main(args: Array[String]): Unit = {
        System.setProperty("play.json.parser.maxNestingDepth", "10000")
        
        val options = CommandlineParser.parseConfig(args)

        val jreLocations = Util.getJRELocations(options)

        Files.createDirectories(options.callGraphsDir)

        val projectSpecPaths =
            Files.list(options.projectsDir)
                .filter { path =>
                    if(path.toString.endsWith(".conf")) {
                        val project = path.getFileName.toString.stripSuffix(".conf")
                        project.startsWith(options.projectFilter) && (options.projects.isEmpty || options.projects.contains(project))
                    } else {
                        false
                    }
                }
                .sorted
                .toScala(List)

        val consoleOut = System.out
        val consoleErr = System.err
        val teeOutputStream = TeeOutputStream(System.out, List())
        val teePrintStream = PrintStream(teeOutputStream)

        try {
            System.setOut(teePrintStream)
            System.setErr(teePrintStream)

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
                        case Action.Analyze => runAnalysis(options, jreLocations, projectSpec, adapter, cgAlgo, callGraphsDirectory, testCase, teeOutputStream)
                        case Action.Assess => assessCallGraph(options, jreLocations, projectSpec, callGraphsDirectory, testCase)
                        case Action.Size => computeCallGraphSize(options, callGraphsDirectory, testCase)
                        case Action.PrecisionRecall => computePrecisionRecall(options, projectSpec, callGraphsDirectory, testCase)
                        case Action.ConvertDynamicCallGraphToCSV => convertDynamicCallGraphToCSV(options, jreLocations, projectSpec, callGraphsDirectory, testCase)
                        case Action.DynamicStackTraces => dynamicStackTraces(options, callGraphsDirectory, testCase)
                }

            }
        } finally {
            System.setOut(consoleOut)
            System.setErr(consoleErr)
        }

    }

    private def runAnalysis(options: CommandlineOptions, jreLocations: Map[Int, Path], projectSpec: ProjectSpecification, adapter: TestAdapter, cgAlgo: String, callGraphsDirectory: Path, testCase: String, teeOutputStream: TeeOutputStream): Any = {
        val callGraphPath =
            if(options.compress)
                callGraphsDirectory.resolve(s"$testCase-callgraph.csv.gz")
            else
                callGraphsDirectory.resolve(s"$testCase-callgraph.csv")

        val logFilePath = callGraphsDirectory.resolve(s"$testCase-log.txt")

        if(! options.overwriteCallgraph && Files.exists(callGraphPath))
            println(s"Call graph file $callGraphPath exists. Do not run analysis.")
        else {
            Using(makeCallGraphWriter(callGraphPath)) { callGraphWriter =>
                redirectedStdoutToLogfile(logFilePath, teeOutputStream) {
                    println(s"running ${adapter.frameworkName} $cgAlgo against ${projectSpec.name}")

                    val future = Future {
                        try {
                            adapter.serializeCG(
                                cgAlgo,
                                projectSpec.target(options.projectsDir.toFile).getCanonicalPath,
                                callGraphWriter,
                                AdapterOptions.makeJavaOptions(
                                    testCase,
                                    callGraphsDirectory,
                                    projectSpec.main.orNull,
                                    projectSpec.allClassPathEntryPaths(options.projectsDir.toFile),
                                    projectSpec.java,
                                    jreLocations(projectSpec.java),
                                    target = projectSpec.target(options.projectsDir.toFile).toString,
                                    jvmArgs = projectSpec.jvm_args.getOrElse(Array.empty[String]),
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
            }.get
        }
    }

    private def convertDynamicCallGraphToCSV(options: CommandlineOptions, jreLocations: Map[Int, Path], projectSpec: ProjectSpecification, callGraphsDirectory: Path, testCase: String): Unit = {
        val dynamicCallGraphJSONPath = callGraphsDirectory.resolve(s"$testCase-callgraph.json.gz")
        val dynamicCallGraphSerialized = Util.readJSON(dynamicCallGraphJSONPath).validate[DynamicJCGAdapter.CallGraphSerialized].get
        val classPath = projectSpec.allClassPathEntryPaths(options.projectsDir.toFile).map(Paths.get(_).toFile)
        val jdkPath = jreLocations(projectSpec.java)
        val updatedCallGraph = dynamicCallGraphSerialized.addDeclaredTargetsToCallSites(classPath, jdkPath).jvmToJavaTypes

        val dynamicCallGraphCSVPath = callGraphsDirectory.resolve(s"$testCase-callgraph.csv.gz")
        Using(OutputStreamWriter(GZIPOutputStream(BufferedOutputStream(FileOutputStream(dynamicCallGraphCSVPath.toFile))))) { writer =>
            updatedCallGraph.deserialize.toReachableMethods.writeCsv(writer)
        }.get
    }

    private def dynamicStackTraces(options: CommandlineOptions, callGraphsDirectory: Path, testCase: String): Unit = {
        val dynamicCallGraphJSONPath = callGraphsDirectory.resolve(s"$testCase-callgraph.json.gz")
        val dynamicCallGraphSerialized = Util.readJSON(dynamicCallGraphJSONPath).validate[DynamicJCGAdapter.CallGraphSerialized].get
        val dynamicCallGraphDeserialized = dynamicCallGraphSerialized.jvmToJavaTypes.deserialize

        for(method <- options.searchedMethods) {
            println(s"========== stack traces for $method ==========")

            for(stackTrace <- dynamicCallGraphDeserialized.stackTraces(method)) {
                println(stackTrace.reverse.map(callSite => callSite.method.toString + ": " + callSite.line).mkString("\n"))
                print("\n")
            }

            print("\n\n")
        }
    }

    private def assessCallGraph(options: CommandlineOptions, jreLocations: Map[Int, Path], projectSpec: ProjectSpecification, callGraphDirectory: Path, testCase: String): Unit = {
        try {

            val callGraphPath = Util.findCallGraphFile(callGraphDirectory, testCase)

            val assessment: Assessment = options.language match {
                case "java" =>
                    val callGraph = Util.readReachableMethods(callGraphPath)

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

        } catch {
            case exc: Throwable =>
                System.err.println(s"Error while processing ${testCase}")
                exc.printStackTrace()
        }
    }

    def computeCallGraphSize(options: CommandlineOptions, callGraphDirectory: Path, testCase: String): Unit = {

        val callGraphPath = Util.findCallGraphFile(callGraphDirectory, testCase)
        val reachableMethods = Util.readReachableMethods(callGraphPath).reachableMethods

        val appMethods = reachableMethods.count { (caller, callSiteMap) =>
            options.reachableMethodsInclude.matches(caller.declaringClass)
        }

        val edgeCount = reachableMethods.foldLeft(0){ case (acc, (method,callSiteMap)) =>
            acc + callSiteMap.foldLeft(0) { case (acc,(callSite,targets)) => acc + targets.size }
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

    def computePrecisionRecall(options: CommandlineOptions, projectSpec: ProjectSpecification, callGraphDirectory: Path, testCase: String): Unit = {
        try {
            val predictedCallGraphPath = Util.findCallGraphFile(callGraphDirectory, testCase)

            val truthCallGraphsDirectory = options.truthCallGraphsDirectory.resolve("Dynamic", "Dynamic")
            val truthCallGraphPath = Util.findCallGraphFile(truthCallGraphsDirectory, testCase)

            println(f"Compare $predictedCallGraphPath (${Files.size(predictedCallGraphPath) / (1024.0 * 1024.0)}%.2fmb) against $truthCallGraphPath (${Files.size(truthCallGraphPath) / (1024.0 * 1024.0)}%.2fmb)")

            val predictedCallGraph = try { Util.readReachableMethods(predictedCallGraphPath) } catch { case exc: Exception => throw new RuntimeException(s"Error while parsing static call graph $predictedCallGraphPath", exc) }
            val truthCallGraph = try { Util.readReachableMethods(truthCallGraphPath) } catch { case exc: Exception => throw new RuntimeException(s"Error while parsing dynamic call graph $truthCallGraphPath", exc) }

            val precisionRecall = PrecisionRecallJava(
                actualCallGraph = truthCallGraph.reachableMethods,
                predictedCallGraph = predictedCallGraph.reachableMethods,
                packageScope = options.comparisonScope match
                    case ComparisonScope.All => Regex(".*")
                    case ComparisonScope.Package => projectSpec.compare_package match
                        case Some(pkg) => Regex(s"$pkg")
                        case None => Regex(".*"),
                reachableMethodsInclude = options.reachableMethodsInclude,
                edgeInclude = options.edgesInclude,
                reachableMethodsExclude = options.reachableMethodsExclude,
                edgeExclude = options.edgesExclude,
                computeFalsePositiveClosureSize = options.falsePositiveClosureSize,
                computeFalseNegativeClosureSize = options.falseNegativeClosureSize
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

            for(scope <- List("methods", "edges", "edges-with-line-numbers");
                metric <- List("true-positives", "false-positives", "false-negatives", "false-positive-boundary", "false-negative-boundary")
                if(!(scope == "methods" && metric == "false-negative-boundary") && !(scope == "methods" && metric == "false-positive-boundary"))
                ) {

                val classification = callGraphDirectory.resolve(s"$testCase-${options.comparisonName}-$scope-$metric.csv.gz")
                Using((GZIPOutputStream(BufferedOutputStream(FileOutputStream(classification.toFile))))) { writer =>
                    scope match {
                        case "methods" =>
                            writeMethods(precisionRecall.methods, metric, writer)
                        case "edges" =>
                            metric match
                                case "false-positive-boundary" => writeBoundary(precisionRecall.edges.falsePositiveBoundary, scope, writer)
                                case "false-negative-boundary" => writeBoundary(precisionRecall.edges.falseNegativeBoundary, scope, writer)
                                case _                         => writeEdges(precisionRecall.edges, scope, metric, writer)
                        case "edges-with-line-numbers" =>
                            metric match
                                case "false-positive-boundary" => writeBoundary(precisionRecall.edgesWithCallSiteLineNumbers.falsePositiveBoundary, scope, writer)
                                case "false-negative-boundary" => writeBoundary(precisionRecall.edgesWithCallSiteLineNumbers.falseNegativeBoundary, scope, writer)
                                case _                         => writeEdges(precisionRecall.edgesWithCallSiteLineNumbers, scope, metric, writer)
                    }
                }
            }
        } catch {
            case exc: Throwable => exc.printStackTrace()
        }
    }

    ///////////////////////////// Helper Functions //////////////////////////////////////

    private def writeMethods(classification: Classification[Method], metric: String, writer: OutputStream): Unit =
        writer.write("method\n".getBytes(StandardCharsets.UTF_8))
        val result = metric match
            case "true-positives"  => classification.truePositive
            case "false-positives" => classification.falsePositive
            case "false-negatives" => classification.falseNegative

        result
            .view
            .toArray
            .sortBy(method => method.toString)
            .foreach(method => writer.write((method.toString + "\n").getBytes(StandardCharsets.UTF_8)))

    private def writeEdges(classification: Classification[Edge], scope: String, metric: String, writer: OutputStream): Unit =
        if(scope.contains("with-line-numbers"))
            writer.write("caller|line|declared-target|target\n".getBytes(StandardCharsets.UTF_8))
        else
            writer.write("caller|declared-target|target\n".getBytes(StandardCharsets.UTF_8))

        val result = metric match
            case "true-positives" => classification.truePositive
            case "false-positives" => classification.falsePositive
            case "false-negatives" => classification.falseNegative

        result
            .view
            .toArray
            .sortBy(edge => (edge.caller.toString, edge.line, edge.target.toString))
            .foreach(edge =>
                if (scope.contains("with-line-numbers"))
                    writer.write(s"${edge.caller}|${edge.line.mkString("")}|${edge.declaredTarget}|${edge.target}\n".getBytes(StandardCharsets.UTF_8))
                else
                    writer.write(s"${edge.caller}|${edge.declaredTarget}|${edge.target}\n".getBytes(StandardCharsets.UTF_8))
            )


    private def writeBoundary(boundary: Map[Edge, TransitiveClosureSize], scope: String, writer: OutputStream): Unit =
        if (scope.contains("with-line-numbers"))
            writer.write("closure-methods|closure-edges|caller|line|declared-target|target\n".getBytes(StandardCharsets.UTF_8))
        else
            writer.write("closure-methods|closure-edges|caller|declared-target|target\n".getBytes(StandardCharsets.UTF_8))

        boundary
            .view
            .toArray
            .sortBy((edge, closureSize) => (closureSize.methods, closureSize.edges, edge.caller.toString, edge.line))(using Ordering[(Long, Long, String, Option[Int])].reverse)
            .foreach((edge, closureSize) =>
                if (scope.contains("with-line-numbers"))
                    writer.write(s"${closureSize.methods}|${closureSize.edges}|${edge.caller}|${edge.line.mkString("")}|${edge.declaredTarget}|${edge.target}\n".getBytes(StandardCharsets.UTF_8))
                else
                    writer.write(s"${closureSize.methods}|${closureSize.edges}|${edge.caller}|${edge.declaredTarget}|${edge.target}\n".getBytes(StandardCharsets.UTF_8))
            )

    private def toJson[T : Writes](classification: Classification[T]): String => JsValue = {
        case "true-positives"  => Json.toJson(classification.truePositive)
        case "false-positives" => Json.toJson(classification.falsePositive)
        case "false-negatives" => Json.toJson(classification.falseNegative)
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
        if(callGraphPath.toString.endsWith(".gz")) {
            OutputStreamWriter(GZIPOutputStream(BufferedOutputStream(FileOutputStream(callGraphPath.toFile))))
        } else {
            OutputStreamWriter(BufferedOutputStream(FileOutputStream(callGraphPath.toFile)))
        }
    }


    class TeeOutputStream(val consoleOut: PrintStream, var targets: List[OutputStream]) extends OutputStream {
        override def write(b: Int): Unit = {
            for (t <- consoleOut +: targets) {
                t.write(b)
            }
        }

        override def write(b: Array[Byte], off: Int, len: Int): Unit = {
            for (t <- consoleOut +: targets) {
                t.write(b, off, len)
            }
        }

        @throws[IOException]
        override def flush(): Unit = {
            for (t <- consoleOut +: targets) {
                t.flush
            }
        }

        @throws[IOException]
        override def close(): Unit = {
            // TeeOutputStream does not own any of the streams. Nothing to close.
        }
    }

    protected def redirectedStdoutToLogfile(logFilePath: Path, teeOutputStream: TeeOutputStream)(run: => Unit): Unit = {
        val logOutputStream = BufferedOutputStream(FileOutputStream(logFilePath.toFile))
        try {
            teeOutputStream.targets = List(logOutputStream)
            run
        } finally {
            teeOutputStream.targets = List()
            logOutputStream.close()
        }
    }
}
