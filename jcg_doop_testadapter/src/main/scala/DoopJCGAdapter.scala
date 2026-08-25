
import java.io.File
import java.io.Writer
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.io.Source
import scala.sys.process.Process
import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import org.opalj.br.ClassType

import scala.collection.compat.immutable.ArraySeq
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
        "0-CFA-REFLECTION",
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
            case "0-CFA" | "0-CFA-REFLECTION" => "context-insensitive"
            case "1-CFA"                      => "1-call-site-sensitive"
            case "1-CFA+HEAP"                 => "1-call-site-sensitive+heap"
            case "1OBJ-CFA"                   => "1-object-sensitive"
            case "1OBJ-CFA+HEAP"              => "1-object-sensitive+heap"
            case "1TYP-CFA"                   => "1-type-sensitive"
            case "1TYP-CFA+HEAP"              => "1-type-sensitive+heap"
            case "1OBJ-1TYP-CFA+HEAP"         => "1-object-1-type-sensitive+heap"
            case _ => throw IllegalArgumentException(s"Unknown call graph algorithm $algorithm")
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
                "--analysis", algorithmToDoopAnalysis(algorithm))
                ++ (if (algorithm.contains("REFLECTION")) Array("--reflection") else Array.empty[String])
                ++ Array(
                "--timeout", "1440",
                "--platform", s"java_$javaVersion",
                "--use-local-java-platform", JDKPath.toAbsolutePath.toString,
                "-i", inputDirPath)
                ++ classPath

            if (analyzeJDK) {
               args ++= JRELocation.getAllJREJars(JDKPath).map(_.toString)
            }

            if (mainClass != null)
                args ++= Array("--main", mainClass)


            println(args.mkString(" "))

            val memoryMiB = (Runtime.getRuntime.maxMemory().toDouble / scala.math.pow(1024,2)).round
            Process(
                args,
                Some(doopHome.toFile),
                "DOOP_HOME" -> doopHome.toAbsolutePath.toString,
                "DOOP_OUT" -> outDir.toAbsolutePath.toString,
                "DEFAULT_JVM_OPTS" -> s"\"-DmaxHeapSize=${memoryMiB}m\" \"-DstackSize=1000m\""
            ).!

            val database = Files.list(outDir).findFirst().get().resolve("database")
            val callGraphCsv = database.resolve("CallGraphEdge.csv")
            val methodInvocationLinesCsv = database.resolve("MethodInvocation-Line.facts")
            val reachableMethods = parseCallGraph(
                callGraphCsv,
                methodInvocationLinesCsv,
                new File(inputDirPath),
                JDKPath.toFile,
                output
            )
            reachableMethods.writeCsv(output)

            val factsGenerationTime = Files.readString(database.resolve("facts-generation-time.txt")).toLong
            val analysisExecutionTime = Files.readString(database.resolve("analysis-execution-time.txt")).toLong
            val totalTime = factsGenerationTime + analysisExecutionTime

            totalTime
        } finally {
            FileUtils.deleteDirectory(outDir.toFile)
        }

    }



    private def parseCallGraph(callGraphPath: Path, methodInvocationLinesPath: Path, tgtJar: File, jreDir: File, output: Writer): ReachableMethods = {
        Using.Manager { use =>
            val methodInvocationCsv = use(Source.fromFile(methodInvocationLinesPath.toFile))
            val methodInvocationLines: Map[String, Int] =
                methodInvocationCsv.getLines().map { methodInvocationLineNumber =>
                    val Array(methodInvocation, lineNumber) = methodInvocationLineNumber.split("\t")
                    (methodInvocation -> lineNumber.toInt)
                }.toMap


            val callGraphCsv = use(Source.fromFile(callGraphPath.toFile))
            parseCallGraph(callGraphCsv, methodInvocationLines)
        }.get
    }

    private def parseCallGraph(doopEdges: Source, methodInvocationLineNumbers: Map[String, Int]): ReachableMethods = {
        val callGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]

        for (line <- doopEdges.getLines()) {
            val Array(_, callerDeclaredTgtNumber, _, tgtStr) = line.split("\t")
            try {
                val (callerStr, declaredTgtStr, numberString) =
                    if (callerDeclaredTgtNumber.contains("native ")) {
                        val Array(callerStr, declaredTgt) = callerDeclaredTgtNumber.split("/")
                        val Array(declardTargetClass, declaredTargetReturnType, declaredTargetMethodSig) = declaredTgt.slice(declaredTgt.indexOf("<") + 1, declaredTgt.indexOf(">")).split(' ')
                        val declaredTarget = declardTargetClass.stripSuffix(":") + "." + declaredTargetMethodSig.split('(')(0)
                        (callerStr, declaredTarget, "0")
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

                val caller = toMethod(callerStr.slice(1, callerStr.length - 1))

                val (declaredClass, declaredMethodName) = declaredTgtStr.splitAt(declaredTgtStr.lastIndexOf("."))
//                val declaredTarget = Method(declaringClass = declaredClass, name = declaredMethodName.drop(1), returnType = "<undefined>", parameterTypes = ArraySeq.empty)
                val declaredTarget = Method(declaringClass = "", name = "", returnType = "", parameterTypes = ArraySeq.empty)
                val line = methodInvocationLineNumbers.getOrElse(callerDeclaredTgtNumber, -1)
                val callSite = CallSite(declaredTarget = declaredTarget, line = line, pc = None)

                val target = toMethod(tgtStr.slice(1, tgtStr.length - 1))

                val currentCallsites = callGraph.getOrElseUpdate(caller, mutable.Map.empty)
                val targets = currentCallsites.getOrElseUpdate(callSite, mutable.Set.empty)
                targets += target
            } catch {
                case e: Throwable ⇒ println(e)
            }

        }

        ReachableMethods(callGraph)
    }

    private def toMethod(methodStr: String): Method = {
        """([^:]+): ([^ ]+) ([^\(]+)\(([^\)]*)\)""".r.findFirstMatchIn(methodStr) match {
            case Some(m) ⇒
                val declClass = m.group(1)
                val returnType = m.group(2)
                val name = m.group(3)
                val params = if (m.group(4).isEmpty) Array.empty[String] else m.group(4).split(",")
                Method(
                    declaringClass = declClass,
                    name = name,
                    returnType = returnType,
                    parameterTypes = ArraySeq.unsafeWrapArray(params)
                )
            case None ⇒ throw new IllegalArgumentException()
        }
    }

}
