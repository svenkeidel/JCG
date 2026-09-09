import pascal.taie.analysis.graph.callgraph.{CallGraph, CallGraphBuilder}
import pascal.taie.{World, WorldBuilder}
import pascal.taie.config.{AnalysisConfig, AnalysisPlanner, ConfigManager, Configs, Options, Plan, PlanConfig, Scope}
import pascal.taie.frontend.java.JavaWorldBuilder
import pascal.taie.ir.stmt.Invoke
import pascal.taie.language.classes.JMethod
import pascal.taie.util.collection.Lists

import java.nio.file.{Files, Path, Paths}
import java.text.ParseException
import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.io.Source
import scala.util.Using
import scala.jdk.CollectionConverters.*
import play.api.libs.json.{Json, __}
import org.apache.commons.io.FileUtils
import pascal.taie.analysis.AnalysisManager


object Tai_e_JCG_Adapter extends JavaTestAdapter {
    override val frameworkName: String = "Taie"

    override val possibleAlgorithms: Array[String] = Array("CHA", "0-CFA", "1-CFA", "1-CFA+HEAP", "1OBJ-CFA", "1OBJ-CFA+HEAP", "1TYP-CFA")

    override def serializeCG(
        algorithm: String,
        target: String,
        output: java.io.Writer,
        adapterOptions: AdapterOptions
    ): AnalysisResult = {

        val mainClass = adapterOptions.getString("mainClass")
        val classPath = adapterOptions.getStringArray("classPath")
        val javaVersion = adapterOptions.getInt("javaVersion")
        val jdkPath = adapterOptions.getPath("JDKPath")
        val analyzeJDK = adapterOptions.getBoolean("analyzeJDK")
        val target = adapterOptions.getString("target")

        val callGraphDirectory = Files.createTempDirectory("tai-e")
        try {

            val cp = ArraySeq.ofRef(classPath).prepended(target)

            val callGraphOptions = algorithm.toUpperCase match {
                case "CHA" => List("--analysis", "cg=algorithm:cha")
                case "0-CFA" => List("--analysis", "cg=algorithm:pta", "--analysis", "pta=cs:ci")
                case "1-CFA" => List("--analysis", "cg=algorithm:pta", "--analysis", "pta=cs:1-call")
                case "1-CFA+HEAP" => List("--analysis", "cg=algorithm:pta", "--analysis", "pta=cs:1-call-1h")
                case "1OBJ-CFA" => List("--analysis", "cg=algorithm:pta", "--analysis", "pta=cs:1-obj")
                case "1OBJ-CFA+HEAP" => List("--analysis", "cg=algorithm:pta", "--analysis", "pta=cs:1-obj-1h")
                case "1TYP-CFA" => List("--analysis", "cg=algorithm:pta", "--analysis", "pta=cs:1-type")
                case "1TYP-CFA+HEAP" => List("--analysis", "cg=algorithm:pta", "--analysis", "pta=pta;1-type-1h")
                case _ => throw new RuntimeException("Invalid algorithm: " + algorithm)
            }
            val command =
                (if(mainClass != null) List("--main-class", mainClass) else List()) ++
                List(
                    "-java", javaVersion.toString,
                    "--jre-dir", if(jdkPath.endsWith("jre")) jdkPath.getParent.toString else jdkPath.toString,
                    "--class-path", cp.mkString(":"),
//                    "-scope", "ALL",
                    "--output-dir", callGraphDirectory.toString
                ) ++ callGraphOptions
            println(command.mkString(" "))

            val options = Options.parse(command*)
            val content = Configs.getAnalysisConfig
            val analysisConfigs = AnalysisConfig.parseConfigs(content)
            val manager: ConfigManager = new ConfigManager(analysisConfigs)
            val planner: AnalysisPlanner = new AnalysisPlanner(manager, options.getKeepResult)
            val configs = PlanConfig.readConfigs(options)
            manager.overwriteOptions(configs)
            val plan = planner.expandPlan(configs, options.getScope == Scope.REACHABLE)

            val builder = new JavaWorldBuilder

            val irGenerationStart = Time()
            builder.build(options)
            val irGenerationEnd = Time()

            val callGraphComputationStart = Time()
            new AnalysisManager(plan).execute()
            val callGraphComputationEnd = Time()

            val taieCallGraph: CallGraph[Invoke,JMethod] = World.get().getResult(CallGraphBuilder.ID);
            val jcgCallGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]

            for(edge <- taieCallGraph.edges().iterator().asScala) {
                val taieCallSite = edge.getCallSite
                val caller = taiToJCGMethod(taieCallGraph.getContainerOf(taieCallSite))
                val target = taiToJCGMethod(edge.getCallee)

                val methodRef = taieCallSite.getInvokeExp.getMethodRef
                val declaredTarget = Method(
                    declaringClass = methodRef.getDeclaringClass.toString,
                    name = methodRef.getName,
                    returnType = methodRef.getReturnType.toString,
                    parameterTypes = methodRef.getParameterTypes.asScala.map(_.toString).to(ArraySeq)
                )
                val callSite = CallSite(declaredTarget = declaredTarget, line = taieCallSite.getLineNumber, pc = None)

                val callSiteMap = jcgCallGraph.getOrElseUpdate(caller, mutable.Map.empty)
                val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
                targets += target

            }

            val reachableMethods = ReachableMethods(jcgCallGraph)
            reachableMethods.writeCsv(output)

            AnalysisResult.Success(
                irGeneration = irGenerationEnd - irGenerationStart,
                callGraphComputation = callGraphComputationEnd - callGraphComputationStart
            )
        } finally {
            FileUtils.deleteDirectory(callGraphDirectory.toFile)
        }
    }

    private def taiToJCGMethod(method: JMethod): Method =
        Method(
            declaringClass = method.getDeclaringClass.toString,
            name = method.getName,
            returnType = method.getReturnType.toString,
            parameterTypes = method.getParamTypes.asScala.map(_.toString).to(ArraySeq)
        )

}
