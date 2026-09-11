import one.profiler.{AsyncProfiler, Counter}
import play.api.libs.json.{JsArray, JsNull, JsObject, JsValue, Json, Writes}

import java.io.{File, Writer}
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using
import jdk.jfr.consumer.{EventStream, RecordedEvent}

trait JavaTestAdapter extends TestAdapter {
    override val language: String = "java"

    type Configuration
    type CallGraph

    def configure[A](
        algorithm: String,
        target: String,
        mainClass: String,
        classPath: Array[String],
        javaVersion: Int,
        jdkPath: Path,
        analyzeJDK: Boolean
    )
    (actionWithConfiguration: Configuration => A): A

    def generateIR(configuration: Configuration): Unit

    def computeCallGraph(configuration: Configuration): CallGraph

    def callGraphToJCG(configuration: Configuration, callGraph: CallGraph): mutable.Map[Method, mutable.Map[CallSite, mutable.Set[Method]]]

    override def warmup(algorithm: String, inputDirPath: String, adapterOptions: AdapterOptions): AnalysisResult =
        val mainClass = adapterOptions.getString("mainClass")
        val classPath = adapterOptions.getStringArray("classPath")
        val javaVersion = adapterOptions.getInt("javaVersion")
        val jdkPath = adapterOptions.getPath("JDKPath")
        val analyzeJDK = adapterOptions.getBoolean("analyzeJDK")
        val target = adapterOptions.getString("target")

        configure(algorithm, target, mainClass, classPath, javaVersion, jdkPath, analyzeJDK) { configuration =>
            generateIR(configuration)
            computeCallGraph(configuration)
            AnalysisResult.Success(JsNull)
        }

    override def measureTime(algorithm: String, inputDirPath: String, adapterOptions: AdapterOptions): AnalysisResult =
        val mainClass = adapterOptions.getString("mainClass")
        val classPath = adapterOptions.getStringArray("classPath")
        val javaVersion = adapterOptions.getInt("javaVersion")
        val jdkPath = adapterOptions.getPath("JDKPath")
        val analyzeJDK = adapterOptions.getBoolean("analyzeJDK")
        val target = adapterOptions.getString("target")

        configure(algorithm, target, mainClass, classPath, javaVersion, jdkPath, analyzeJDK) { configuration =>

            Time.settleDown()

            val irGenerationStart = Time()
            generateIR(configuration)
            val irGenerationEnd = Time()

            Time.settleDown()

            val callGraphComputationStart = Time()
            val callGraph = computeCallGraph(configuration)
            val callGraphComputationEnd = Time()

            AnalysisResult.Success(Json.obj(
                "irGeneration" -> (irGenerationEnd - irGenerationStart),
                "callGraphComputation" -> (callGraphComputationEnd - callGraphComputationStart)
            ))
        }

    override def measureMemory(algorithm: String, inputDirPath: String, adapterOptions: AdapterOptions): AnalysisResult =
        val mainClass = adapterOptions.getString("mainClass")
        val classPath = adapterOptions.getStringArray("classPath")
        val javaVersion = adapterOptions.getInt("javaVersion")
        val jdkPath = adapterOptions.getPath("JDKPath")
        val analyzeJDK = adapterOptions.getBoolean("analyzeJDK")
        val target = adapterOptions.getString("target")
        val outputDirectory = adapterOptions.getPath("outputDirectory")
        val testCase = adapterOptions.getString("testCase")

        val profiler = AsyncProfiler.getInstance()

        val profilingOutput = outputDirectory.resolve(s"$testCase-alloc.jfr")
        profiler.execute(s"start,jfr,event=alloc,file=$profilingOutput")

        configure(algorithm, target, mainClass, classPath, javaVersion, jdkPath, analyzeJDK) { configuration =>
            generateIR(configuration)

            val callGraph = computeCallGraph(configuration)

            profiler.execute("stop")

            Using(EventStream.openFile(profilingOutput)) { eventStream =>

                val adapterClassName = this.getClass.getName
                val methodNames = Array("configure", "generateIR", "computeCallGraph")
                val allocations: Array[Long] = methodNames.map(_ => 0)

                def sumBytes(event: RecordedEvent) = {
                    event.getStackTrace.getFrames.asScala.find(frame =>
                        frame.getMethod.getType.getName == adapterClassName && methodNames.contains(frame.getMethod.getName)
                    ) match
                        case Some(frame) =>
                            val methodIdx = methodNames.indexOf(frame.getMethod.getName)
                            allocations(methodIdx) = allocations(methodIdx) + event.getLong("tlabSize")
                        case None => {}
                }

                eventStream.onEvent("jdk.ObjectAllocationInNewTLAB", sumBytes)
                eventStream.onEvent("jdk.ObjectAllocationOutsideTLAB", sumBytes)
                eventStream.start()

                AnalysisResult.Success(Json.toJson(methodNames.zip(allocations).toMap))
            }.get

        }

    override def serializeCG(algorithm: String, inputDirPath: String, output: Writer, adapterOptions: AdapterOptions): AnalysisResult =
        val mainClass = adapterOptions.getString("mainClass")
        val classPath = adapterOptions.getStringArray("classPath")
        val javaVersion = adapterOptions.getInt("javaVersion")
        val jdkPath = adapterOptions.getPath("JDKPath")
        val analyzeJDK = adapterOptions.getBoolean("analyzeJDK")
        val target = adapterOptions.getString("target")

        configure(algorithm, target, mainClass, classPath, javaVersion, jdkPath, analyzeJDK) { configuration =>
            generateIR(configuration)
            val callGraph = computeCallGraph(configuration)
            val jcgCallGraph = callGraphToJCG(configuration, callGraph)
            val reachableMethods = ReachableMethods(jcgCallGraph)
            reachableMethods.writeCsv(output)
            AnalysisResult.Success(Json.obj("methods" -> reachableMethods.methods.size, "edges" -> reachableMethods.edges.size))
        }
}
