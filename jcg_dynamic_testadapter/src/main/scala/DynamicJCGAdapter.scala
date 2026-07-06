import org.apache.commons.io.IOUtils
import org.opalj.br.{ClassType, FieldAccessMethodHandle, FieldType, FieldTypes, MethodCallMethodHandle, MethodDescriptor, ReturnType}
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.*

import java.io.{BufferedInputStream, File, FileInputStream, Writer}
import java.nio.file.{Files, Path, Paths}
import java.util.zip.GZIPInputStream
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Using
import play.api.libs.json.{JsResult, JsValue, Json, Reads, Writes, __}
import play.api.libs.functional.syntax.*

import java.net.URL
import java.nio.charset.StandardCharsets
import scala.collection.immutable.ArraySeq

type OuterCallSite = CallSite
object DynamicJCGAdapter extends JavaTestAdapter {

    override val possibleAlgorithms: Array[String] = Array("Dynamic")

    override val frameworkName: String = "Dynamic"

    val port = 1337

    override def serializeCG(
        algorithm:      String,
        inputDirPath:   String,
        output:         Writer,
        adapterOptions: AdapterOptions = AdapterOptions.makeEmptyOptions()
    ): Long = {
        val mainClass = adapterOptions.getString("mainClass")
        var classPath = List.from(adapterOptions.getStringArray("classPath"))
        val JDKPath = adapterOptions.getPath("JDKPath")
        val jvmArgs = adapterOptions.getStringArray("jvmArgs")
        val programArgs = adapterOptions.getStringArray("analysisArgs")

        val javaPath = {
            // JDK > 8
            if(Files.exists(JDKPath.resolve("bin", "java")))
                JDKPath.resolve("bin", "java")
            // JDK <= 8
            else if (Files.exists(JDKPath.getParent.resolve("bin", "java")))
                JDKPath.getParent.resolve("bin", "java")
            else
                throw java.io.IOException(s"Cannot find java exectuble in $JDKPath")
        }
        val agentPath = Paths.get("jcg_dynamic_testadapter", "src", "main", "resources", "DynamicCG.so")

        val callGraphPath = Files.createTempFile("callgraph", ".json.gz")

        try {
            val agentArgs = Array(callGraphPath.toString).mkString(",")
            classPath :+= inputDirPath

            val reachableMethods = mutable.Set[Method]()
            val edges = mutable.Map[Method, mutable.Map[(Int, Int), mutable.Set[Method]]]()

            var args = List(javaPath.toAbsolutePath.toString)
            args :+= s"-Xmx${Runtime.getRuntime.maxMemory()}"
            args ++= jvmArgs
            args :+= s"-agentpath:${agentPath.toAbsolutePath}=$agentArgs"
            args ++= List("-cp", classPath.mkString(":"))
            args :+= mainClass
            args ++= programArgs

            println(args.mkString(" "))

            val processBuilder = new ProcessBuilder(args.asJava).inheritIO()

            val usrLib = Paths.get("/usr/lib/x86_64-linux-gnu")
            val libBoostPath = Files.walk(usrLib, 1).filter(lib => Files.isRegularFile(lib) && lib.getFileName.toString.startsWith("libboost_iostreams")).findFirst().toScala

            libBoostPath match {
                case Some(libBoost) =>
                    println(s"Found libboost_iostreams: ${libBoost}")
                    processBuilder.environment().put("LD_LIBRARY_PATH", libBoost.getParent.toString)
                case None => throw java.io.IOException("Cannot find boost library path")
            }
            println(s"LD_LIBRARY_PATH=${processBuilder.environment().get("LD_LIBRARY_PATH")}")

            // For finding memory leaks
//            processBuilder.environment().put("LD_PRELOAD", "/usr/lib/x86_64-linux-gnu/libasan.so.8")
//            processBuilder.environment().put("ASAN_OPTIONS", "detect_leaks=1:allow_user_segv_handler=1")

            val before = System.nanoTime
            processBuilder.start().waitFor()
            val after = System.nanoTime

            println(s"Read call graph from $callGraphPath with ${Files.size(callGraphPath).toDouble / math.pow(10, 6)}MB")
            val callGraphJSON = Using(GZIPInputStream(BufferedInputStream(FileInputStream(callGraphPath.toFile)))) {
                input =>
                    val callGraph = Json.parse(input.readAllBytes()).validate[CallGraphSerialized].get
                    val json = Json.toJson(callGraph.addDeclaredTargetsToCallSites(classPath.map(jar => Paths.get(jar).toFile).toArray, JDKPath))
                    output.write(Json.prettyPrint(json))
            }

            after - before
        } finally {
            Files.delete(callGraphPath)
        }
    }

    case class CallSiteSerialized(method: String, line: Int, pc: Int, declaredTarget: Option[Method]):
        def deserialize(methods: Map[String,Method]): CallSite =
            CallSite(method = methods(method), line = line, pc = pc, declaredTarget)

        def addDeclaredTarget(instruction: InvocationInstruction): CallSiteSerialized =
            instruction match {
                case invoke: INVOKESTATIC =>
                    this.copy(declaredTarget =
                        Some(Method(
                            declaringClass = invoke.declaringClass.toJVMTypeName,
                            name = invoke.name,
                            returnType = invoke.methodDescriptor.returnType.toJVMTypeName,
                            parameterTypes = invoke.methodDescriptor.parameterTypes.map(_.toJVMTypeName).toList
                        ))
                    )

                case invoke: INVOKESPECIAL =>
                    this.copy(declaredTarget =
                        Some(Method(
                            declaringClass = invoke.declaringClass.toJVMTypeName,
                            name = invoke.name,
                            returnType = invoke.methodDescriptor.returnType.toJVMTypeName,
                            parameterTypes = invoke.methodDescriptor.parameterTypes.map(_.toJVMTypeName).toList
                        ))
                    )

                case invoke: INVOKEVIRTUAL =>
                    this.copy(declaredTarget =
                        Some(Method(
                            declaringClass = invoke.declaringClass.toJVMTypeName,
                            name = invoke.name,
                            returnType = invoke.methodDescriptor.returnType.toJVMTypeName,
                            parameterTypes = invoke.methodDescriptor.parameterTypes.map(_.toJVMTypeName).toList
                        ))
                    )

                case invoke: INVOKEINTERFACE =>
                    this.copy(declaredTarget =
                        Some(Method(
                            declaringClass = invoke.declaringClass.toJVMTypeName,
                            name = invoke.name,
                            returnType = invoke.methodDescriptor.returnType.toJVMTypeName,
                            parameterTypes = invoke.methodDescriptor.parameterTypes.map(_.toJVMTypeName).toList
                        ))
                    )
                case invoke: INVOKEDYNAMIC =>
                    this.copy(declaredTarget =
                        Some(Method(
                            declaringClass =
                                invoke.bootstrapMethod.handle match {
                                    case handle: MethodCallMethodHandle => handle.receiverType.toJVMTypeName
                                    case handle: FieldAccessMethodHandle => s"${handle.declaringClassType.toJVMTypeName}.${handle.name}"
                                },
                            name = invoke.name,
                            returnType = invoke.methodDescriptor.returnType.toJVMTypeName,
                            parameterTypes = invoke.methodDescriptor.parameterTypes.map(_.toJVMTypeName).toList
                        ))
                    )
            }

    case class CallTreeSerialized(callTree: Map[String, CallTreeSerialized]):
        def deserialize(callSites: Map[String,CallSite]): CallTree =
            CallTree(callTree.map((callSite,subTree) => (callSites(callSite), subTree.deserialize(callSites))))

    case class CallGraphSerialized(callTree: CallTreeSerialized, callSites: Map[String,CallSiteSerialized], methods: Map[String,Method]):
        def deserialize: CallTree =
            val deserializedCallSites = callSites.view.mapValues(_.deserialize(methods)).toMap
            callTree.deserialize(deserializedCallSites)

        def addDeclaredTargetsToCallSites(classPath: Array[File], jdkPath: Path): CallGraphSerialized =
            val jreJars = JRELocation.getAllJREJars(jdkPath).map(_.toFile)
            val project: Project[URL] = Project(classPath ++ jreJars.toArray, Array.empty[File])

            val updatedCallSites = callSites.view.mapValues(callSite =>
                val callingMethod = methods(callSite.method)

                try {
                    val callingClass = toClassType(callingMethod.declaringClass)
                    val classFile = project.classFile(callingClass).getOrElse(throw IllegalArgumentException(s"class ${callingMethod.declaringClass} not found"))
                    val returnType = ReturnType(callingMethod.returnType);
                    val parameterTypes = scala.collection.compat.immutable.ArraySeq(callingMethod.parameterTypes.map(FieldType.apply): _*);
                    val md = MethodDescriptor(parameterTypes, returnType);
                    val method = classFile.findMethod(callingMethod.name, md).getOrElse(throw IllegalArgumentException(s"method ${callingMethod.declaringClass}.$callingMethod not found"))
                    val code = method.body.getOrElse(throw IllegalArgumentException(s"No body for method ${callingMethod.declaringClass}.$callingMethod"))
                    val pcAndInstruction = code.iterator.find(instruction => instruction.pc == callSite.pc).getOrElse(throw IllegalArgumentException(s"instruction not found at pc ${callSite.pc}"))
                    val invokationInstruction = pcAndInstruction.instruction.asInstanceOf[InvocationInstruction]
                    callSite.addDeclaredTarget(invokationInstruction)
                } catch {
                    case exception: Throwable =>
                        System.err.println(s"Cannot find declared callsite of ${callingMethod.declaringClass}.${callingMethod}:${callSite.pc}: ${exception.getMessage()}")
                        if(exception.getMessage.strip().equals("null"))
                            exception.printStackTrace(System.err)
                        callSite
                }

            ).toMap

            this.copy(callSites = updatedCallSites)

        private def toClassType(jvmRefType: String): ClassType = {
            assert(jvmRefType.length > 2)
            ClassType(jvmRefType.substring(1, jvmRefType.length - 1))
        }


    implicit val callSiteSerializedReads: Reads[CallSiteSerialized] = Json.reads[CallSiteSerialized]
    implicit val callTreeSerializedReads: Reads[CallTreeSerialized] = (json: JsValue) =>
        implicitly[Reads[Map[String, CallTreeSerialized]]].reads(json).map(CallTreeSerialized(_))
    implicit val callGraphSerializedReads: Reads[CallGraphSerialized] = Json.reads[CallGraphSerialized]

    implicit val callSiteSerializedWrites: Writes[CallSiteSerialized] = Json.writes[CallSiteSerialized]
    implicit val callTreeSerializedWrites: Writes[CallTreeSerialized] = (callTree: CallTreeSerialized) => Json.toJson(callTree.callTree)
    implicit val callGraphSerializedWrites: Writes[CallGraphSerialized] = Json.writes[CallGraphSerialized]

    case class CallSite(method: Method, line: Int, pc: Int, declaredTarget: Option[Method])
    case class CallTree(callSites: Map[CallSite, CallTree]):
        def toReachableMethods: ReachableMethods =
            val reachableMethods = mutable.Map.empty[Method, mutable.Map[CallSite, Set[Method]]]
            addReachableMethods(reachableMethods)
            ReachableMethods(reachableMethods.view.map((method, callSites) =>
                ReachableMethod(method, callSites.view.map((callSite, targets) =>
                    new OuterCallSite(declaredTarget = callSite.declaredTarget.orNull, line = callSite.line, pc = Some(callSite.pc), targets = targets)
                ).toSet)
            ).toSet)

        private def addReachableMethods(reachableMethods: mutable.Map[Method, mutable.Map[CallSite, Set[Method]]]): Unit =
            for ((callSite, subTree) <- callSites) {

                val method = callSite.method
                val methodCallSites = reachableMethods.getOrElse(callSite.method, mutable.Map.empty[CallSite, Set[Method]])
                val methodTargets = methodCallSites.getOrElse(callSite, Set.empty)

                val newTargets = subTree.callSites.keySet.map(_.method)
                methodCallSites += callSite -> (methodTargets ++ newTargets)
                reachableMethods += method -> (methodCallSites)

                subTree.addReachableMethods(reachableMethods)
            }


    implicit val callTreeReads: Reads[CallTree] = (json: JsValue) =>
        callGraphSerializedReads.reads(json).map(_.deserialize)
}
