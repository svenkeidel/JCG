
import java.io.File
import java.io.Writer
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.io.Source
import scala.sys.process.Process
import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import org.opalj.br.ClassType

import scala.util.Using
import scala.math.pow

/**
 * This is an experimental stage [[JavaTestAdapter]] as it is not possible to run Doop without
 * installing it (and a data-log engine).
 * Therefore, this object has the capability of converting the output of the CallGraphEdge table
 * into the [[ReachableMethods]] data-format.
 *
 * @author Florian Kuebler
 */
object DoopAdapter extends JavaTestAdapter {

    val frameworkName: String = "Doop"

    val possibleAlgorithms: Array[String] = Array(
        "0-CFA",
        "1-CFA",
        "1-CFA+HEAP",
        "1OBJ-CFA",
        "1OBJ-CFA+HEAP",
        "1TYP-CFA",
        "1TYP-CFA+HEAP",
        "1OBJ-1TYP-CFA+HEAP"
//        No idea what these are. I can't find documentation
//        "types-only",
//        "adaptive-2-object-sensitive+heap",
//        "context-insensitive-plus",
//        "context-insensitive-plusplus",
//        "basic-only",
//        "blacklist-1-object-sensitive+heap",
//        "data-flow",
//        "dependency-analysis",
//        "fully-guided-context-sensitive",
//        "micro",
//        "oracular-precision",
//        "oracular-scalability",
//        "partitioned-2-object-sensitive+heap",
//        "selective-2-object-sensitive+heap",
//        "sound-may-point-to",
//        "sticky-2-object-sensitive",
//        "types-only",
//        "xtractor"
    )

    private def algorithmToDoopAnalysis(algorithm: String): String =
        algorithm match {
            case "0-CFA"              => "context-insensitive"
            case "1-CFA"              => "1-call-site-sensitive"
            case "1-CFA+HEAP"         => "1-call-site-sensitive+heap"
            case "1OBJ-CFA"           => "1-object-sensitive"
            case "1OBJ-CFA+HEAP"      => "1-object-sensitive+heap"
            case "1TYP-CFA"           => "1-type-sensitive"
            case "1TYP-CFA+HEAP"      => "1-type-sensitive+heap"
            case "1OBJ-1TYP-CFA+HEAP" => "1-object-1-type-sensitive+heap"
            case _ => throw IllegalArgumentException(s"Unknown call graph algorithm $algorithm")
        }


    private def createJsonRepresentation(
                                            callGraphPath: Path, methodInvocationLinesPath: Path, tgtJar: File, jreDir: File, output: Writer
    ): Unit = {
        Using.Manager { use =>
            val methodInvocationCsv = use(Source.fromFile(methodInvocationLinesPath.toFile))
            val methodInvocationLines: Map[String, Int] =
                methodInvocationCsv.getLines().map { methodInvocationLineNumber =>
                    val Array(methodInvocation, lineNumber) = methodInvocationLineNumber.split("\t")
                    (methodInvocation -> lineNumber.toInt)
                }.toMap


            val callGraphCsv = use(Source.fromFile(callGraphPath.toFile))
            val callGraph = extractDoopCG(callGraphCsv, methodInvocationLines)

            output.write(Json.stringify(Json.toJson(callGraph)))
        }
    }

    private def extractDoopCG(doopEdges: Source, methodInvocationLineNumbers: Map[String, Int]): ReachableMethods = {
        val callGraph = mutable.Map.empty[String, mutable.Map[(String, Int), mutable.Set[String]]].withDefault(_ ⇒ mutable.HashMap.empty.withDefault(_ ⇒ mutable.Set.empty))

        for (line <- doopEdges.getLines()) {
            val Array(_, callerDeclaredTgtNumber, _, tgtStr) = line.split("\t")
            try {
                val (callerStr, declaredTgt, numberString) =
                    if (callerDeclaredTgtNumber.contains("native ")) {
                        val Array(callerStr, declaredTgt) = callerDeclaredTgtNumber.split("/")
                        (callerStr, declaredTgt, "0")
                    } else if ("<main-thread-init>/0" == callerDeclaredTgtNumber) {
                        ("<java.lang.Thread: java.lang.Thread currentThread()>", "java.lang.Thread.<init>", "0")
                    } else if ("<thread-group-init>/0" == callerDeclaredTgtNumber) {
                        ("<java.lang.Thread: java.lang.Thread currentThread()>", "java.lang.ThreadGroup.<init>", "0")
                    } else if (callerDeclaredTgtNumber.startsWith("<register-finalize")) {
                        val array = callerDeclaredTgtNumber.drop("<register-finalize ".length).dropRight("  >".length).split("/")
                        (array(array.length - 3), array(array.length - 2), array(array.length - 1))
                    } else {
                        val Array(callerStr, declaredTgt, numberString) = callerDeclaredTgtNumber.split("/")
                        (callerStr, declaredTgt, numberString)
                    }
                val caller = callerStr.slice(1, callerStr.length - 1)
                val tgt = tgtStr.slice(1, tgtStr.length - 1)
                val number = methodInvocationLineNumbers.getOrElse(callerDeclaredTgtNumber, -1)

                val currentCallsites = callGraph(caller)
                val callSite = declaredTgt -> number
                val currentCallees = currentCallsites(callSite)

                currentCallees += tgt
                currentCallsites += (callSite -> currentCallees)
                callGraph += (caller -> currentCallsites)
            } catch {
                case e: Throwable ⇒
                    println(e)
            }

        }


        ReachableMethods(
            callGraph.iterator.map((caller, callSites) =>
                ReachableMethod(toMethod(caller),
                    callSites.iterator.map { case ((declaredTarget, lineNumber), targets) =>
                        val (declaredClass, declaredMethodName) = declaredTarget.splitAt(declaredTarget.lastIndexOf("."))
                        val declaredMethod = Method(name = declaredMethodName, declaringClass = declaredClass, returnType = "", parameterTypes = List())
                        CallSite(declaredMethod, lineNumber, None, targets.map(toMethod).toSet)
                    }.toSet
                )
            ).toSet
        )
    }

    private def toMethod(methodStr: String): Method = {
        """([^:]+): ([^ ]+) ([^\(]+)\(([^\)]*)\)""".r.findFirstMatchIn(methodStr) match {
            case Some(m) ⇒
                val declClass = m.group(1)
                val returnType = m.group(2)
                val name = m.group(3)
                val params = if (m.group(4).isEmpty) Array.empty[String] else m.group(4).split(",")
                Method(
                    name, toJVMType(declClass), toJVMType(returnType), params.map(toJVMType).toList
                )
            case None ⇒ throw new IllegalArgumentException()
        }
    }

    private def toJVMType(t: String): String = {
        if (t.endsWith("[]"))
            s"[${toJVMType(t.substring(0, t.length - 2))}"
        else t match {
            case "byte"    ⇒ "B"
            case "short"   ⇒ "S"
            case "int"     ⇒ "I"
            case "long"    ⇒ "J"
            case "float"   ⇒ "F"
            case "double"  ⇒ "D"
            case "boolean" ⇒ "Z"
            case "char"    ⇒ "C"
            case "void"    ⇒ "V"
            case _         ⇒ s"L${t.replace(".", "/")};"

        }
    }

    private def toClassType(jvmRefType: String): ClassType = {
        assert(jvmRefType.length > 2)
        ClassType(jvmRefType.substring(1, jvmRefType.length - 1))
    }

    override def serializeCG(
        algorithm:      String,
        inputDirPath:   String,
        output:         Writer,
        adapterOptions: AdapterOptions
    ): Long = {
        val env = System.getenv

        val mainClass = adapterOptions.getString("mainClass")
        val classPath = adapterOptions.getStringArray("classPath")
        val JDKPath = adapterOptions.getPath("JDKPath")
        val javaVersion = adapterOptions.getInt("javaVersion")
        val analyzeJDK = adapterOptions.getBoolean("analyzeJDK")

        assert(env.containsKey("DOOP_HOME"))
        val doopHome = Paths.get(env.get("DOOP_HOME"))
        assert(Files.exists(doopHome))
        assert(Files.isDirectory(doopHome))


        val outDir = Files.createTempDirectory(null)

        try {

            var args = Array(
                "./bin/doop",
                "--analysis", algorithmToDoopAnalysis(algorithm),
//                "--reflection",
                "--timeout", "1440",
                "--platform", s"java_$javaVersion",
                "--use-local-java-platform", JDKPath.toAbsolutePath.toString,
                "-i", inputDirPath) ++ classPath

            if (analyzeJDK) {
               args ++= JRELocation.getAllJREJars(JDKPath).map(_.toString)
            }

            if (mainClass != null)
                args ++= Array("--main", mainClass)


            println(args.mkString(" "))

            val memoryMiB = (Runtime.getRuntime.maxMemory().toDouble / scala.math.pow(1024,2)).round
            val before = System.nanoTime()
            Process(
                args,
                Some(doopHome.toFile),
                "DOOP_HOME" -> doopHome.toAbsolutePath.toString,
                "DOOP_OUT" -> outDir.toAbsolutePath.toString,
                "DEFAULT_JVM_OPTS" -> s"\"-DmaxHeapSize=${memoryMiB}m\" \"-DstackSize=1000m\""
            ).!
            val after = System.nanoTime()

            val database = Files.list(outDir).findFirst().get().resolve("database")
            val callGraphCsv = database.resolve("CallGraphEdge.csv")
            val methodInvocationLinesCsv = database.resolve("MethodInvocation-Line.facts")
            createJsonRepresentation(
                callGraphCsv,
                methodInvocationLinesCsv,
                new File(inputDirPath),
                JDKPath.toFile,
                output
            )

            after - before
        } finally {
            FileUtils.deleteDirectory(outDir.toFile)
        }

    }
}
