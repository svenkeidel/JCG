import java.io.Writer
import java.nio.file.{Files, Path}
import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import sootup.callgraph.CallGraph
import sootup.callgraph.CallGraphAlgorithm
import sootup.callgraph.ClassHierarchyAnalysisAlgorithm
import sootup.callgraph.RapidTypeAnalysisAlgorithm
import sootup.core.cache.provider.FullCacheProvider
import sootup.core.inputlocation.AnalysisInputLocation
import sootup.core.model.SourceType
import sootup.core.signatures.MethodSignature
import sootup.core.types.VoidType
import sootup.java.bytecode.frontend.inputlocation.*
import sootup.java.core.views.{JavaView, LoadingStrategy}


object SootUpJCGAdapter extends JavaTestAdapter {

    private val CHA = "CHA"
    private val RTA = "RTA"

    val possibleAlgorithms: Array[String] = Array(CHA, RTA)

    val frameworkName: String = "SootUp"

    override type Configuration = SootUpConfiguration

    class SootUpConfiguration(val mainClass: String, val inputLocations: List[AnalysisInputLocation], var view: JavaView = null, val algorithm: JavaView => CallGraphAlgorithm)

    override def configure[A](
         algorithm: String,
         target: String,
         mainClass: String,
         classPath: Array[String],
         javaVersion: Int,
         jdkPath: Path,
         analyzeJDK: Boolean)
         (actionWithConfiguration: Configuration => A): A = {

        val jreInputLocation = {
            if(javaVersion <= 8) {
                if(Files.exists(jdkPath.resolve("jre", "lib", "rt.jar")))
                    ArchiveBasedAnalysisInputLocation(jdkPath.resolve("jre", "lib", "rt.jar"), SourceType.Library)
                else if (Files.exists(jdkPath.resolve("lib", "rt.jar")))
                    ArchiveBasedAnalysisInputLocation(jdkPath.resolve("lib", "rt.jar"), SourceType.Library)
                else throw java.io.IOException("Cannot find rt.jar")
            } else {
                CustomJrtFileSystemAnalysisInputLocation(jdkPath.resolve("lib", "modules"), SourceType.Library)
            }
        }
        val inputLocations = List(
                JavaClassPathAnalysisInputLocation(target),
                jreInputLocation
            ) ++
            classPath.map(JavaClassPathAnalysisInputLocation(_)).toList

        val algo = (view: JavaView) => algorithm match {
            case CHA => new ClassHierarchyAnalysisAlgorithm(view)
            case RTA => new RapidTypeAnalysisAlgorithm(view)
        }

        val config = SootUpConfiguration(
            mainClass = mainClass,
            inputLocations = inputLocations,
            view = null,
            algorithm = algo)

        actionWithConfiguration(config)
    }

    override def generateIR(configuration: SootUpConfiguration): Unit =
        configuration.view = new JavaView(configuration.inputLocations.asJava, new FullCacheProvider, LoadingStrategy.eager())


    override type CallGraph = sootup.callgraph.CallGraph

    override def computeCallGraph(configuration: Configuration): CallGraph =
        if (configuration.mainClass == null) {
            configuration.algorithm(configuration.view).initialize()
        } else {
            val idFactory = configuration.view.getIdentifierFactory
            val mainClassType = idFactory.getClassType(configuration.mainClass)
            val stringArrayType = idFactory.getType("java.lang.String[]")
            val mainMethod = idFactory.getMethodSignature(mainClassType, "main", VoidType.getInstance(), List(stringArrayType).asJava)
            configuration.algorithm(configuration.view).initialize(List(mainMethod).asJava)
        }

    override def callGraphToJCG(configuration: Configuration, sootUpCallGraph: CallGraph): mutable.Map[Method, mutable.Map[CallSite, mutable.Set[Method]]] = {
        val jcgCallGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]

        for (sootUpCaller <- sootUpCallGraph.getMethodSignatures.asScala;
             caller = sootMethodToJCGMethod(sootUpCaller);
             call <- sootUpCallGraph.callsFrom(sootUpCaller).asScala
             ) {
            val stmt = call.invokableStmt

            // e.g. null for finalize and no invoke for static initializers
            val declaredTarget = if (stmt != null && stmt.getInvokeExpr.isPresent) {
                sootMethodToJCGMethod(stmt.getInvokeExpr.get().getMethodSignature)
            } else {
                Method(declaringClass = "", name = "", returnType = "", parameterTypes = ArraySeq.empty)
            }

            val lineNumber = call.getLineNumber

            val callSite = CallSite(
                declaredTarget = declaredTarget,
                line = lineNumber,
                pc = None
            )

            val target = sootMethodToJCGMethod(call.targetMethodSignature)

            val callSiteMap = jcgCallGraph.getOrElseUpdate(caller, mutable.Map.empty)
            val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
            targets += target
        }

        jcgCallGraph
    }

    private def sootMethodToJCGMethod(method: MethodSignature): Method = {
        val name = method.getName
        val declaringClass = method.getDeclClassType.toString
        val returnType = method.getType.toString
        val paramTypes = method.getParameterTypes.asScala.map(t => t.toString)
        Method(name = name, declaringClass = declaringClass, returnType = returnType, parameterTypes = ArraySeq.from(paramTypes))
    }
}
