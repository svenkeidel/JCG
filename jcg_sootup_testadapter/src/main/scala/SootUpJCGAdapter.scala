import java.io.File
import java.io.Writer
import scala.collection.JavaConverters.*
import scala.collection.mutable
import play.api.libs.json.Json
import qilin.driver.PTAFactory
import qilin.driver.PTAPattern
import qilin.pta.PTAConfig
import sootup.callgraph.CallGraph
import sootup.callgraph.CallGraphAlgorithm
import sootup.callgraph.ClassHierarchyAnalysisAlgorithm
import sootup.callgraph.RapidTypeAnalysisAlgorithm
import sootup.core.inputlocation.AnalysisInputLocation
import sootup.core.model.SourceType
import sootup.core.signatures.MethodSignature
import sootup.core.types.ArrayType
import sootup.core.types.ClassType
import sootup.core.types.PrimitiveType.BooleanType
import sootup.core.types.PrimitiveType.ByteType
import sootup.core.types.PrimitiveType.CharType
import sootup.core.types.PrimitiveType.DoubleType
import sootup.core.types.PrimitiveType.FloatType
import sootup.core.types.PrimitiveType.IntType
import sootup.core.types.PrimitiveType.LongType
import sootup.core.types.PrimitiveType.ShortType
import sootup.core.types.Type
import sootup.core.types.VoidType
import sootup.java.bytecode.frontend.inputlocation.*
import sootup.java.core.views.JavaView

import java.nio.file.{Files, Paths}
import scala.collection.compat.immutable.ArraySeq

object SootUpJCGAdapter extends JavaTestAdapter {

    private val CHA = "CHA"
    private val RTA = "RTA"

    val possibleAlgorithms: Array[String] = Array(CHA, RTA)

    val frameworkName: String = "SootUp"
    def serializeCG(
        algorithm:      String,
        inputDirPath:   String,
        output:         Writer,
        adapterOptions: AdapterOptions
    ): Long = {
        val mainClass = adapterOptions.getString("mainClass")
        val classPath = adapterOptions.getStringArray("classPath")
        val JDKPath = adapterOptions.getPath("JDKPath")
        val analyzeJDK = adapterOptions.getBoolean("analyzeJDK")
        val javaVersion = adapterOptions.getInt("javaVersion")

        val jreInputLocation = {
            if(javaVersion <= 8) {
                if(Files.exists(JDKPath.resolve("jre", "lib", "rt.jar")))
                    ArchiveBasedAnalysisInputLocation(JDKPath.resolve("jre", "lib", "rt.jar"), SourceType.Library)
                else if (Files.exists(JDKPath.resolve("lib", "rt.jar")))
                    ArchiveBasedAnalysisInputLocation(JDKPath.resolve("lib", "rt.jar"), SourceType.Library)
                else throw java.io.IOException("Cannot find rt.jar")
            } else {
                CustomJrtFileSystemAnalysisInputLocation(JDKPath.resolve("lib", "modules"), SourceType.Library)
            }
        }
        val inputLocations = List(JavaClassPathAnalysisInputLocation(inputDirPath), jreInputLocation)
            ++ classPath.map(JavaClassPathAnalysisInputLocation(_)).toList

        val view = new JavaView(inputLocations.asJava)

        // todo no-bodies-for-excluded in case of !analyzeJDK

        def computeCG(cgAlgorithm: CallGraphAlgorithm): CallGraph = {
            val cg =
                if (mainClass == null) {
                cgAlgorithm.initialize()
            } else {
                val idFactory = view.getIdentifierFactory
                val mainClassType = idFactory.getClassType(mainClass)
                val stringArrayType = idFactory.getType("java.lang.String[]")
                val mainMethod = idFactory.getMethodSignature(mainClassType, "main", VoidType.getInstance(), List(stringArrayType).asJava)
                cgAlgorithm.initialize(List(mainMethod).asJava)
            }
            cg
        }

        val before = System.nanoTime
        val sootUpCallGraph: CallGraph = algorithm match {
            case CHA => computeCG(new ClassHierarchyAnalysisAlgorithm(view))
            case RTA => computeCG(new RapidTypeAnalysisAlgorithm(view))
        }
        val after = System.nanoTime

        val jcgCallGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]

        for(sootUpCaller <- sootUpCallGraph.getMethodSignatures.asScala;
            caller = sootMethodToJCGMethod(sootUpCaller);
            call <- sootUpCallGraph.callsFrom(sootUpCaller).asScala
        ) {
            val stmt = call.getInvokableStmt

            // e.g. null for finalize and no invoke for static initializers
            val declaredTarget = if (stmt != null && stmt.containsInvokeExpr()) {
                stmt.getInvokeExpr.get().getMethodSignature
            } else
                call.getTargetMethodSignature

            val lineNumber =
                if (stmt != null)
                    stmt.getPositionInfo.getStmtPosition.getFirstLine
                else
                    -1

            val callSite = CallSite(
                declaredTarget = sootMethodToJCGMethod(declaredTarget),
                line = lineNumber,
                pc = None
            )

            val target = sootMethodToJCGMethod(call.getTargetMethodSignature)

            val callSiteMap = jcgCallGraph.getOrElseUpdate(caller, mutable.Map.empty)
            val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
            targets += target
        }

        ReachableMethods(jcgCallGraph).writeCsv(output)

        after - before
    }

    private def sootMethodToJCGMethod(method: MethodSignature): Method = {
        val name = method.getName
        val declaringClass = method.getDeclClassType.toString
        val returnType = method.getType.toString
        val paramTypes = method.getParameterTypes.asScala.map(t => t.toString)
        Method(name = name, declaringClass = declaringClass, returnType = returnType, parameterTypes = ArraySeq.from(paramTypes))
    }
}
