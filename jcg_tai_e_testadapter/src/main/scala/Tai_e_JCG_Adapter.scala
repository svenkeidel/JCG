import org.apache.commons.io.FileUtils

import java.nio.file.{Files, Path, Paths}
import java.lang.ProcessBuilder
import java.text.ParseException

import scala.collection.compat.immutable.ArraySeq
import scala.io.Source
import scala.util.Using

import play.api.libs.json.Json

object Tai_e_JCG_Adapter extends JavaTestAdapter {
    override val frameworkName: String = "Taie"

    override val possibleAlgorithms: Array[String] = Array("CHA", "PTA")

    override def serializeCG(
        algorithm: String,
        target: String,
        output: java.io.Writer,
        adapterOptions: AdapterOptions
    ): Long = {

        val mainClass = adapterOptions.getString("mainClass")
        val classPath = adapterOptions.getStringArray("classPath")
        val jdkPath = adapterOptions.getPath("JDKPath")
        val analyzeJDK = adapterOptions.getBoolean("analyzeJDK")
        val target = adapterOptions.getString("target")

        val callGraphDirectory = Files.createTempDirectory("tai-e")
        try {

            val algoTaieName = algorithm.toUpperCase match {
                case "CHA" => "cha"
                case "PTA" => "pta"
                case _ => throw new RuntimeException("Invalid algorithm: " + algorithm)
            }

            val taieJarPath = Paths.get("jcg_tai_e_testadapter", "src", "main", "resources", "tai-e-all-0.5.4-SNAPSHOT.jar").toAbsolutePath

            val cp = ArraySeq.ofRef(classPath).prepended(target)

            val command = List(
                "java",
                "-jar", taieJarPath.toString,
                "--main-class", mainClass,
                "-java", "8",
                "--jre-dir", jdkPath.toString,
                "--class-path", cp.mkString(":"),
                "-scope", "ALL",
                "-a", "cg=algorithm:" + algoTaieName + ";dump:true;dump-methods:true",
                "--output-dir", callGraphDirectory.toString,
            )
            println(command.mkString(" "))

            val start = System.nanoTime()
            val exitCode = new ProcessBuilder(command*).inheritIO().start().waitFor()
            val end = System.nanoTime()
            if(exitCode != 0)
                throw IllegalArgumentException(s"Exit code $exitCode not 0")

            val callGraphPath = callGraphDirectory.resolve("call-graph.dot")
            val methods = parseDotNodes(callGraphPath)
            val edges = parseDotEdges(callGraphPath, methods)

            output.write(Json.prettyPrint(Json.toJson(edges)))

            start - end
        } finally {
            FileUtils.deleteDirectory(callGraphDirectory.toFile)
        }
    }

    private def parseDotEdges(callGraphPath: Path, methods: Map[Long, Method]): ReachableMethods =
        val EdgeMatch = """\s*"\d+"\s*->\s*"\d+"\s*\[label.*""".r
        val EdgePattern = """\s*"(\d+)"\s*->\s*"(\d+)"\s*\[label="\[(\d+)@L(-?\d+)\][^<]*(<[^;]*);",\];\s*""".r

        var callGraph: Map[Method, Map[Int, CallSite]] = Map.empty

        Using(Source.fromFile(callGraphPath.toString)) { dot =>
            for (line <- dot.getLines();
                 if(EdgeMatch.matches(line))) {
                line match
                    case EdgePattern(from, to, pcStr, lineNumberStr, restSignature) =>
                        val fromMethod = methods(from.toLong)
                        val toMethod = methods(to.toLong)
                        val pc = pcStr.toInt
                        val lineNumber = lineNumberStr.toInt
                        val (declaredSignature, _) = restSignature.splitAt(restSignature.lastIndexOf(">"))
                        val newCallSite = CallSite(
                            declaredTarget = parseMethodSignature(declaredSignature),
                            line = lineNumber,
                            pc = Some(pc),
                            targets = Set.empty
                        )
                        val callSites = callGraph.getOrElse(fromMethod, Map(pc -> newCallSite))
                        val currentCallSite = callSites.getOrElse(pc, newCallSite)
                        callGraph += fromMethod -> (
                            callSites + (pc ->
                                currentCallSite.copy(targets = currentCallSite.targets + toMethod)
                                )
                            )
                    case _ => throw ParseException(s"Cannot parse edge ${line}", 0)
            }
        }.get

        ReachableMethods(
            callGraph.view.map((method,callSites) => ReachableMethod(method,callSites.values.toSet)).toSet
        )

    private def parseDotNodes(callGraphPath: Path): Map[Long,Method] = {

        val NodeMatch = """\s*"\d+"\s*\[label.*""".r
        val NodePattern = """\s*"(\d+)"\s*\[label="([^"]+)",\];\s*""".r

        Using(Source.fromFile(callGraphPath.toString)) { dot =>
            val result =
                for(line <- dot.getLines();
                    if(NodeMatch.matches(line)))
                yield (
                    line match
                        case NodePattern(id, label) => id.toLong -> parseMethodSignature(label)
                        case _ => throw ParseException(s"Cannot parse dot node ${line}", 0)
                )
            result.toMap
        }.get
    }



    /**
     * Convert method signature from Tai-e format to the JVM format used by JCG
     *
     * @param sig Tai-e method signature as string (e.g. `<cfne.Demo: void main(java.lang.String[])>`)
     * @return Method object similar to other JCG adapters
     */
    private def parseMethodSignature(signature: String): Method = {
        val Array(className, methodSignature) = signature.stripPrefix("<").stripSuffix(">").split(": ")
        val (returnType, methodAndParams) = methodSignature.splitAt(methodSignature.indexOf(' '))
        val (methodName, params) = methodAndParams.splitAt(methodAndParams.indexOf("("))
        val parameterTypes = params.stripPrefix("(").stripSuffix(")").split(",").map(toJVMType).toList

        Method(
            name = methodName.strip(),
            declaringClass = toJVMType(className),
            returnType = toJVMType(returnType),
            parameterTypes = parameterTypes
        )
    }

    /**
     * Convert Type string (e.g. `java.lang.String[]`) to NVM internal format used
     * by JCG (e.g. `[Ljava.lang.String;`)
     * Also used for class names (e.g. `cfne.Demo` becomes `Lcfne/Demo;`)
     *
     * @param javaType
     * @return
     */
    private def toJVMType(javaType0: String): String = {
        var javaType = javaType0

        var dims: Int = 0
        while (javaType.endsWith("[]")) {
            dims += 1
            javaType = javaType.substring(0, javaType.length - 2)
        }
        val base: String = javaType match {
            case "byte" => "B"
            case "char" => "C"
            case "double" => "D"
            case "float" => "F"
            case "int" => "I"
            case "long" => "J"
            case "short" => "S"
            case "boolean" => "Z"
            case "void" => "V"
            case _ => "L" + javaType.replace('.', '/') + ";"
        }
        "[".repeat(dims) + base
    }
}
