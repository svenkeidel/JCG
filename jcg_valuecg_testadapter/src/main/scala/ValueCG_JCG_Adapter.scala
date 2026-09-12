import com.google.gson.{Gson, GsonBuilder}
import org.apache.commons.io.FileUtils
import org.jcg.valuecgadapter.ValueCG_TestAdapterImpl.MethodTarget
import org.jcg.valuecgadapter.{SerializedCallgraph, ValueCG_TestAdapterImpl}
import play.api.libs.json.{JsNull, Json}

import java.io.{File, FileInputStream, FileOutputStream, IOException, Writer}
import java.nio.file.{Files, Path, Paths}
import java.util.zip.{ZipEntry, ZipOutputStream}
import java.util.{ArrayList, Arrays, HashMap, HashSet, Map, Set}
import scala.collection.compat.immutable.ArraySeq
import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import scala.math.Numeric.IntIsIntegral
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Using

object ValueCG_JCG_Adapter extends JavaTestAdapter {
    val ALGO_PRECISE = "precise"
    val ALGO_FAST = "fast"
    val ALGO_PRECISE_SERVER_CONF = "valdroidcg-precise.conf" // relative to config directory

    // defined in VALUECG_CONFIG_DIR
    // env variable
    val ALGO_FAST_SERVER_CONF = "valdroidcg-fast.conf"

    override val frameworkName: String = "ValueCG"
    override val possibleAlgorithms: Array[String] = Array(ALGO_PRECISE, ALGO_FAST)

    override def warmup(algorithm: String, inputDirPath: String, adapterOptions: AdapterOptions): AnalysisResult = AnalysisResult.Success(JsNull)
    override def measureTime(algorithm: String, inputDirPath: String, adapterOptions: AdapterOptions): AnalysisResult =
        val mainClass = adapterOptions.getString("mainClass")
        val classPath = adapterOptions.getStringArray("classPath")
        val javaVersion = adapterOptions.getInt("javaVersion")
        val jdkPath = adapterOptions.getPath("JDKPath")
        val analyzeJDK = adapterOptions.getBoolean("analyzeJDK")
        val target = adapterOptions.getString("target")

        configure(algorithm, target, mainClass, classPath, javaVersion, jdkPath, analyzeJDK) { configuration =>
            parseClassFilesAndGenerateIR(configuration)
            val callGraph = computeCallGraph(configuration)
            val timing = Time.fromNanoseconds(Files.readString(configuration.outDir.resolve("ValDroid-timing.txt")).toLong)
            AnalysisResult.irGenerationAndCallGraphComputationTime(Time.zero, timing)
        }


    override type Configuration = ValueCGConfig
    case class ValueCGConfig(command: List[String], runnerDir: String, configDir: String, outDir: Path)

    override def configure[A](
         algorithm: String,
         target: String,
         mainClass: String,
         classPath: Array[String],
         javaVersion: Int,
         jdkPath: Path,
         analyzeJDK: Boolean)
    (actionWithConfiguration: Configuration => A): A =

        val runnerDir = Option(System.getenv("VALUECG_RUNNER_DIR")).getOrElse(throw new IllegalStateException("VALUECG_RUNNER_DIR env variable not set"))
        val configDir = Option(System.getenv("VALUECG_CONFIG_DIR")).getOrElse(throw new IllegalStateException("VALUECG_CONFIG_DIR env variable not set"))

        val inputFile = new File(target)

        val outDir = Files.createTempDirectory("output-cgs")

        // Read and convert generated callgraph files// Read and convert generated callgraph files
        try {
            val out = outDir.toAbsolutePath
            Files.createDirectories(out)

            // Generate configuration file from template
            val templateFile = algorithm match {
                case ALGO_FAST => ALGO_FAST_SERVER_CONF
                case ALGO_PRECISE => ALGO_PRECISE_SERVER_CONF
                case _ => throw new RuntimeException("Invalid algorithm: " + algorithm)
            }
            val templatePath = Paths.get(configDir, templateFile)
            var configContent = new String(Files.readAllBytes(templatePath))
            configContent = configContent.replace("OUTPUT", out.toString)

            if (mainClass != null) configContent += "\n\nJavaAnalyzer.EntryPoint=" + "<" + mainClass + ": void main(java.lang.String[])>"
            else configContent += "\n\nJavaAnalyzer.ValueFinder.Static.CG.LibraryMode=true"

            configContent += s"\nSootImporter.Java.JRE.Path=${jdkPath.toAbsolutePath}"

            val analysisConfig = out.resolve("analysis.conf")
            Files.write(analysisConfig, configContent.getBytes)

            println("\nAnalysis Config:")
            println(configContent)
            println()
            println()

            val zip = zipJars(inputFile, classPath)

            try {

                val command = List("./AnalysisStandaloneRunner", "--configfile", analysisConfig.toString, zip.toString)

                actionWithConfiguration(ValueCGConfig(command = command, runnerDir = runnerDir, configDir = configDir, outDir = outDir))

            } finally Files.delete(zip)

        } catch {
            case e: Exception => throw new RuntimeException("Failed to process " + inputFile, e)
        } finally {
            FileUtils.deleteDirectory(outDir.toFile)
        }

    override def parseClassFilesAndGenerateIR(configuration: Configuration): Unit = {}

    override type CallGraph = SerializedCallgraph

    override def computeCallGraph(configuration: Configuration): CallGraph = {
        // Execute analysis process
        val pb = new ProcessBuilder(configuration.command*)
        pb.directory(new File(configuration.runnerDir))
        pb.redirectErrorStream(true)

        val process = pb.start
        process.getInputStream.transferTo(System.out)

        val exitCode = process.waitFor
        if (exitCode != 0) throw new RuntimeException("Analysis failed with exit code: " + exitCode)

        SerializedCallgraph.readFromFileCompressed(configuration.outDir.resolve("ValDroid.json.gz").toFile)
    }

    override def callGraphToJCG(configuration: Configuration, valueCgCallGraph: CallGraph): mutable.Map[Method, mutable.Map[CallSite, mutable.Set[Method]]] = {
        val callGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]

        for (edge <- valueCgCallGraph.edges.asScala) {
            try {
                edge.normalize()

                val sourceMethod = parseMethodSignature(edge.sourceMethod).getOrElse(throw new RuntimeException(s"Cannot parse source method ${edge.sourceMethod}"))
                val targetMethod = parseMethodSignature(edge.targetMethod).getOrElse(throw new RuntimeException(s"Cannot parse target method ${edge.targetMethod}"))

                val declaredTarget =
                    extractDeclaredSignature(edge.sourceStatement)
                        .flatMap(parseMethodSignature)
                        .getOrElse(Method(name = "", declaringClass = "", returnType = "", parameterTypes = ArraySeq.empty))

                val lineNumber = if (edge.lineNumber != null) edge.lineNumber.toInt else -1

                val callSite = CallSite(declaredTarget = declaredTarget, line = lineNumber, pc = None)

                val callSiteMap = callGraph.getOrElseUpdate(sourceMethod, mutable.Map.empty)
                val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
                targets += targetMethod

            } catch {
                case e: Exception =>
                    System.err.println("Parsing error for edge: " + edge)
                    e.printStackTrace()
            }
        }

        callGraph
    }

    private def extractDeclaredSignature(statement: String): Option[String] = {
        val start = statement.indexOf('<')
        val end = statement.indexOf('>')
        if (start != -(1) && end != -(1)) Some(statement.substring(start, end + 1))
        else None
    }


    private def parseMethodSignature(sig0: String): Option[Method] = {
        try {
            // Remove angle brackets
            val sig = sig0.substring(1, sig0.length - 1)
            // Split into class and method parts
            val colonIdx = sig.indexOf(':')
            val className = sig.substring(0, colonIdx).trim
            val methodPart = sig.substring(colonIdx + 1).trim
            // Extract return type
            val lastSpace = methodPart.lastIndexOf(' ')
            val returnType = methodPart.substring(0, lastSpace).trim
            val rest = methodPart.substring(lastSpace + 1).trim
            // Extract method name and parameters
            val parenIdx = rest.indexOf('(')
            val methodName = rest.substring(0, parenIdx).trim
            val paramsStr = rest.substring(parenIdx + 1, rest.length - 1).trim
            // Parse parameter types
            val paramTypes = paramsStr.split(',').map(_.trim)

            Some(Method(
                name = methodName,
                declaringClass = className,
                returnType = returnType,
                parameterTypes = ArraySeq.unsafeWrapArray(paramTypes)
            ))
        } catch {
            case _: Exception =>
                None
        }

    }

    @throws[IOException]
    private def zipJars(inputFile: File, classPath: Array[String]) = {
        val zipPath = Files.createTempFile("project_", ".zip")
        Using(new ZipOutputStream(new FileOutputStream(zipPath.toFile))) { zipOut =>
            for (jar <- inputFile.getAbsolutePath +: classPath.toList) {
                val jarFile = new File(jar)
                Using(new FileInputStream(jarFile)) { fis =>
                    val zipEntry = new ZipEntry(jarFile.getName)
                    zipOut.putNextEntry(zipEntry)
                    val bytes = new Array[Byte](1024)
                    var length = 0
                    while {
                        length = fis.read(bytes)
                        length >= 0
                    } do {
                        zipOut.write(bytes, 0, length)
                    }
                }

            }
        }
        zipPath
    }
}
