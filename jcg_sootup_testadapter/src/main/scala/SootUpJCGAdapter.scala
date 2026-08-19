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


        def computeCG(cgAlgorithm: CallGraphAlgorithm): (CallGraph, Iterable[MethodSignature]) = {
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
            (cg, cg.getEntryMethods.asScala)
        }

        val before = System.nanoTime

        val (cg: CallGraph, entrypoints: Iterable[MethodSignature]) = algorithm match {
            case CHA => computeCG(new ClassHierarchyAnalysisAlgorithm(view))
            case RTA => computeCG(new RapidTypeAnalysisAlgorithm(view))
        }

        val after = System.nanoTime

        val worklist = mutable.Queue(entrypoints.toSeq*)
        val processed = mutable.Set(worklist.toSeq*)

        var reachableMethods = Set.empty[ReachableMethod]

        while (worklist.nonEmpty) {
            val currentMethod = worklist.dequeue()

            var callSitesMap = Map.empty[(MethodSignature, Int), Set[MethodSignature]]
            for (edge ← cg.callsFrom(currentMethod).asScala) {
                val stmt = edge.getInvokableStmt

                // e.g. null for finalize and no invoke for static initializers
                val declaredMethod = if (stmt != null && stmt.containsInvokeExpr()) {
                    stmt.getInvokeExpr.get().getMethodSignature
                } else
                    edge.getTargetMethodSignature

                val lineNumber =
                    if (stmt != null)
                        stmt.getPositionInfo.getStmtPosition.getFirstLine
                    else
                        -1

                val tgt = edge.getTargetMethodSignature
                val key =
                    if (declaredMethod.getName == tgt.getName)
                        declaredMethod → lineNumber
                    else
                        tgt → lineNumber

                val tgts = callSitesMap.getOrElse(key, Set.empty)
                callSitesMap = callSitesMap.updated(key, tgts + tgt)
                if (!processed.contains(tgt)) {
                    worklist += tgt
                    processed += tgt
                }
            }

            val callSites = callSitesMap.map {
                case ((declaredTgt, line), tgts) ⇒
                    // todo: would be good to have the PC
                    CallSite(createMethodObject(declaredTgt), line, None, tgts.map(createMethodObject))
            }.toSet

            val method = createMethodObject(currentMethod)
            reachableMethods += ReachableMethod(method, callSites)
        }

        output.write(Json.stringify(Json.toJson(ReachableMethods(reachableMethods))))

        after - before
    }

    private def createMethodObject(method: MethodSignature): Method = {
        val name = method.getName
        val declaringClass = javaToJVMType(method.getDeclClassType)
        val returnType = javaToJVMType(method.getType)
        val paramTypes = method.getParameterTypes.asScala.map(t => javaToJVMType(t)).toList

        Method(name, declaringClass, returnType, paramTypes)
    }

    private def javaToJVMType(javaType: Type): String = {
        javaType match {
            case t: ClassType => "L" + t.getFullyQualifiedName.replace('.', '/') + ";"
            case t: ArrayType => "[" * t.getDimension + javaToJVMType(t.getBaseType)
            case _: ByteType => "B"
            case _: CharType => "C"
            case _: DoubleType => "D"
            case _: FloatType => "F"
            case _: ShortType => "S"
            case _: BooleanType => "Z"
            case _: IntType => "I"
            case _: LongType => "J"
            case _: VoidType => "V"
            case _   => throw new IllegalArgumentException(s"Unknow type $javaType")
        }
    }
}
