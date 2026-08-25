import org.apache.commons.io.FileUtils

import java.nio.file.{Files, Path, Paths}
import java.lang.ProcessBuilder
import java.text.ParseException
import scala.collection.compat.immutable.ArraySeq
import scala.io.Source
import scala.util.Using
import play.api.libs.json.Json

import scala.collection.mutable

object Tai_e_JCG_Adapter extends JavaTestAdapter {
    override val frameworkName: String = "Taie"

    override val possibleAlgorithms: Array[String] = Array("CHA", "0-CFA", "1-CFA", "1-CFA+HEAP", "1OBJ-CFA", "1OBJ-CFA+HEAP", "1TYP-CFA")

    override def serializeCG(
        algorithm: String,
        target: String,
        output: java.io.Writer,
        adapterOptions: AdapterOptions
    ): Long = {

        val mainClass = adapterOptions.getString("mainClass")
        val classPath = adapterOptions.getStringArray("classPath")
        val javaVersion = adapterOptions.getInt("javaVersion")
        val jdkPath = adapterOptions.getPath("JDKPath")
        val analyzeJDK = adapterOptions.getBoolean("analyzeJDK")
        val target = adapterOptions.getString("target")

        val callGraphDirectory = Files.createTempDirectory("tai-e")
        try {

            val taieJarPath = Paths.get("jcg_tai_e_testadapter", "src", "main", "resources", "tai-e-all-0.5.5-SNAPSHOT.jar").toAbsolutePath

            val cp = ArraySeq.ofRef(classPath).prepended(target)

            val commonCallGraphOptions = "dump-call-edges:true"
            val callGraphOptions = algorithm.toUpperCase match {
                case "CHA" => List("--analysis", "cg=algorithm:cha;"+commonCallGraphOptions)
                case "0-CFA" => List("--analysis", "cg=algorithm:pta;"+commonCallGraphOptions, "--analysis", "pta=cs:ci")
                case "1-CFA" => List("--analysis", "cg=algorithm:pta;"+commonCallGraphOptions, "--analysis", "pta=cs:1-call")
                case "1-CFA+HEAP" => List("--analysis", "cg=algorithm:pta;"+commonCallGraphOptions, "--analysis", "pta=cs:1-call-1h")
                case "1OBJ-CFA" => List("--analysis", "cg=algorithm:pta;"+commonCallGraphOptions, "--analysis", "pta=cs:1-obj")
                case "1OBJ-CFA+HEAP" => List("--analysis", "cg=algorithm:pta;"+commonCallGraphOptions, "--analysis", "pta=cs:1-obj-1h")
                case "1TYP-CFA" => List("--analysis", "cg=algorithm:pta;"+commonCallGraphOptions, "--analysis", "pta=cs:1-type")
                case "1TYP-CFA+HEAP" => List("--analysis", "cg=algorithm:pta;"+commonCallGraphOptions, "--analysis", "pta=pta;1-type-1h")
                case _ => throw new RuntimeException("Invalid algorithm: " + algorithm)
            }
            val command = List(
                "java",
                s"-Xmx${Runtime.getRuntime.maxMemory()}",
                "-jar", taieJarPath.toString) ++
                (if(mainClass != null) List("--main-class", mainClass) else List()) ++
                List("-java", javaVersion.toString,
                "--jre-dir", if(jdkPath.endsWith("jre")) jdkPath.getParent.toString else jdkPath.toString,
                "--class-path", cp.mkString(":"),
                "-scope", "ALL",
                "--output-dir", callGraphDirectory.toString,
            ) ++ callGraphOptions
            println(command.mkString(" "))

            val exitCode = new ProcessBuilder(command*).inheritIO().start().waitFor()
            if(exitCode != 0)
                throw IllegalArgumentException(s"Exit code $exitCode not 0")

            val reachableMethods = parseCallGraph(callGraphDirectory.resolve("call-edges.csv"))
            reachableMethods.writeCsv(output)

            Files.readString(callGraphDirectory.resolve("timing.txt")).toLong
        } finally {
            FileUtils.deleteDirectory(callGraphDirectory.toFile)
        }
    }

    private def parseCallGraph(callGraphPath: Path): ReachableMethods =
        Using(Source.fromFile(callGraphPath.toString)) { csvSource =>

            val callGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]

            for(line <- csvSource.getLines().drop(1)) {

                val Array(callingContextStr,callingMethodStr,callSiteIdStr,callSiteLineNumberStr,declaredTargetStr,targetContextStr,targetMethodStr) = line.split('|')
//                val callingContext = parseContext(Json.parse(callingContextStr)).get
                val caller = parseMethodSignature(callingMethodStr)
                val callSiteId = callSiteIdStr.toInt
                val callSiteLineNumber = callSiteLineNumberStr.toInt
                val declaredTarget = parseMethodSignature(declaredTargetStr)
//                val targetContext = Json.parse(targetContextStr)
                val target = parseMethodSignature(targetMethodStr)
                val callSite = CallSite(
                    declaredTarget = declaredTarget,
                    line = callSiteLineNumber,
                    pc = None
                )


                val callSiteMap = callGraph.getOrElseUpdate(caller, mutable.Map.empty)
                val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
                targets += target
            }

            ReachableMethods(callGraph)
        }.get

    /**
     * Convert method signature from Tai-e format to the format used by JCG
     *
     * @param sig Tai-e method signature as string (e.g. `<cfne.Demo: void main(java.lang.String[])>`)
     * @return Method object similar to other JCG adapters
     */
    private def parseMethodSignature(signature: String): Method = {
        val Array(className, methodSignature) = signature.stripPrefix("<").stripSuffix(">").split(": ")
        val (returnType, methodAndParams) = methodSignature.splitAt(methodSignature.indexOf(' '))
        val (methodName, params) = methodAndParams.splitAt(methodAndParams.indexOf("("))
        val parameterTypes = params.stripPrefix("(").stripSuffix(")").split(",")

        Method(
            name = methodName.strip(),
            declaringClass = className,
            returnType = returnType,
            parameterTypes = ArraySeq.unsafeWrapArray(parameterTypes)
        )
    }
}
