import java.io.File
import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import soot.G
import soot.PackManager
import soot.Scene
import soot.SootMethod
import soot.options.Options

import java.nio.file.Path
import scala.collection.immutable.ArraySeq

object SootJCGAdapter extends JavaTestAdapter {

    private val CHA = "CHA"
    private val RTA = "RTA"
    private val VTA = "VTA"
    private val Spark = "SPARK"

    val possibleAlgorithms: Array[String] = Array(CHA, RTA, VTA, Spark)

    val frameworkName: String = "Soot"

    override type Configuration = soot.G

    override def configure[A](
         algorithm: String,
         target: String,
         mainClass: String,
         classPath: Array[String],
         javaVersion: Int,
         jdkPath: Path,
         analyzeJDK: Boolean)
     (actionWithConfiguration: Configuration => A): A =
        G.reset()
        val g = G.v()
        val o = g.soot_options_Options()
        o.set_whole_program(true)
        o.set_keep_line_number(true)
        o.set_keep_offset(true)
        o.set_allow_phantom_refs(true)
        o.set_include_all(analyzeJDK)

        val jreJars = JRELocation.getAllJREJars(jdkPath).map(_.toString)

        if (analyzeJDK && algorithm == "CHA") {
            o.set_process_dir((List(target) ++ classPath ++ jreJars).asJava)
        } else {
            o.set_process_dir((List(target) ++ classPath).asJava)
        }

        o.set_soot_classpath((classPath ++ jreJars).mkString(File.pathSeparator))

        o.set_output_format(Options.output_format_none)

        //        o.setPhaseOption("jb", "use-original-names:true")
        o.setPhaseOption("jb", "model-lambdametafactory-namingstrategy:bytecodeoffset")

        o.setPhaseOption("cg", "safe-forname:false")
        o.setPhaseOption("cg", "safe-newinstance:false")
        //o.setPhaseOption("cg", "types-for-invoke:true")

        if (mainClass == null) {
            o.setPhaseOption("cg", "library:signature-resolution")
            o.setPhaseOption("cg", "all-reachable:true")
        } else {
            o.set_main_class(mainClass)
        }

        if (algorithm.contains(CHA)) {
            o.setPhaseOption("cg.cha", "enabled:true")
            o.setPhaseOption("cg.spark", "enabled:false")
        } else if (algorithm.contains(RTA)) {
            o.setPhaseOption("cg.spark", "enabled:true")
            o.setPhaseOption("cg.spark", "vta:false")
            o.setPhaseOption("cg.spark", "rta:true")
            o.setPhaseOption("cg.spark", "on-fly-cg:false")
            o.setPhaseOption("cg.spark", "simulate-natives:true")
        } else if (algorithm.contains(VTA)) {
            o.setPhaseOption("cg.spark", "enabled:true")
            o.setPhaseOption("cg.spark", "rta:false")
            o.setPhaseOption("cg.spark", "vta:true")
            o.setPhaseOption("cg.spark", "simulate-natives:true")
        } else if (algorithm.contains(Spark)) {
            o.setPhaseOption("cg.spark", "enabled:true")
            o.setPhaseOption("cg.spark", "rta:false")
            o.setPhaseOption("cg.spark", "vta:false")
            o.setPhaseOption("cg.spark", "simulate-natives:true")
        } else {
            throw new IllegalArgumentException(s"unknown algorithm $algorithm")
        }

        val scene = g.soot_Scene()
        scene.releaseCallGraph()
        scene.releaseReachableMethods()
        scene.releasePointsToAnalysis()
        scene.releaseActiveHierarchy()
        scene.releaseFastHierarchy()

        try {
            actionWithConfiguration(g)
        } finally {
            G.reset()
        }

    override def parseClassFilesAndGenerateIR(configuration: Configuration): Unit =
        configuration.soot_Scene().loadNecessaryClasses()
        configuration.soot_PackManager().runBodyPacks()

    override type CallGraph = soot.jimple.toolkits.callgraph.CallGraph

    override def computeCallGraph(configuration: Configuration): CallGraph = {
        configuration.soot_PackManager().runPacks()
        configuration.soot_Scene().getCallGraph
    }

    override def callGraphToJCG(configuration: Configuration, sootCallGraph: CallGraph): mutable.Map[Method, mutable.Map[CallSite, mutable.Set[Method]]] = {
        val jcgCallGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]

        for (edge <- sootCallGraph.asScala) {

            val caller = sootMethodToJCGMethod(edge.src())

            val stmt = edge.srcStmt()

            // e.g. null for finalize and no invoke for static initializers
            val declaredTarget = if (stmt != null && stmt.containsInvokeExpr())
                stmt.getInvokeExpr.getMethod
            else
                edge.tgt()

            val lineNumber =
                if (stmt != null)
                    stmt.getJavaSourceStartLineNumber
                else
                    -1

            val callSite = CallSite(
                declaredTarget = sootMethodToJCGMethod(declaredTarget),
                line = lineNumber,
                pc = None
            )

            val target = sootMethodToJCGMethod(edge.tgt)

            val callSiteMap = jcgCallGraph.getOrElseUpdate(caller, mutable.Map.empty)
            val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
            targets += target
        }

        jcgCallGraph
    }

    private def sootMethodToJCGMethod(method: SootMethod): Method = {
        val name = method.getName
        val declaringClass = method.getDeclaringClass.getType.toString
        val returnType = method.getReturnType.toString
        val paramTypes = method.getParameterTypes.asScala.map(_.toString)

        Method(name = name, declaringClass = declaringClass, returnType = returnType, parameterTypes = ArraySeq.from(paramTypes))
    }
}
