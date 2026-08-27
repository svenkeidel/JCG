import com.fasterxml.jackson.core.JsonFactory
import org.apache.commons.io.IOUtils
import org.opalj.br.{Attributes, ClassType, FieldAccessMethodHandle, FieldType, FieldTypes, MethodCallMethodHandle, MethodDescriptor, ReturnType}
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.*

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileInputStream, FileOutputStream, IOException, OutputStream, OutputStreamWriter, Writer}
import java.nio.file.{Files, Path, Paths}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Using
import play.api.libs.json.*
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
        val testCase = adapterOptions.getString("testCase")
        val outputDirectory = adapterOptions.getPath("outputDirectory")
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
        val crashLog = Files.createTempFile("crash", ".log")

        try {
            val agentArgs = Array(callGraphPath.toString).mkString(",")
            classPath :+= inputDirPath

            val reachableMethods = mutable.Set[Method]()
            val edges = mutable.Map[Method, mutable.Map[(Int, Int), mutable.Set[Method]]]()

            var args = List(javaPath.toAbsolutePath.toString)
            args :+= s"-Xmx${Runtime.getRuntime.maxMemory()}"
            args :+= s"-XX:-ClassUnloading" // It is important to disable class unloading, such that method ids remain valid.
            args :+= s"-XX:ErrorFile=${crashLog.toString}"
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
            val exitCode = processBuilder.start().waitFor()
            println(s"Dynamic Callgraph Process Exit Code: $exitCode")
            val after = System.nanoTime

            println(s"Read call graph from $callGraphPath with ${Files.size(callGraphPath).toDouble / math.pow(10, 6)}MB")
            Using(GZIPInputStream(BufferedInputStream(FileInputStream(callGraphPath.toFile)))) { input =>

                // Add declared targets and write json to file.
                val callGraphSerialized = Json.parse(input).validate[DynamicJCGAdapter.CallGraphSerialized].get
                val updatedCallGraph = callGraphSerialized
                    .addDeclaredTargetsToCallSites(classPath.map(File(_)).toArray, JDKPath)

                Using(OutputStreamWriter(GZIPOutputStream(BufferedOutputStream(FileOutputStream(outputDirectory.resolve(s"$testCase-callgraph.json.gz").toFile))))) { jsonWriter =>
                    writeJson(jsonWriter, Json.toJson(updatedCallGraph))
                }

                // Additionally serialize as CSV
                val reachableMethods = updatedCallGraph
                    .jvmToJavaTypes
                    .deserialize
                    .toReachableMethods
                reachableMethods.writeCsv(output)
            }.get

            after - before
        } catch {
            case exc: Throwable =>
                exc.printStackTrace(System.err)
                throw exc
        } finally {
            Files.delete(callGraphPath)

            try {
                val content = Files.readString(crashLog)
                System.err.println(content)
            } catch {
                case e: IOException => // Ignore exception in case no crash log was produced.
            }
        }
    }

    case class CallSiteSerialized(method: String, line: Int, pc: Int, declaredTarget: Option[Method]):
        def deserialize(methods: Map[String,Method]): CallSite =
            CallSite(method = methods(method), line = line, pc = pc, declaredTarget)

        def addDeclaredTarget(instruction: Instruction): CallSiteSerialized =
            instruction match {
                case MethodInvocationInstruction(dc, _, name, desc) =>
                    this.copy(declaredTarget = Some(Method(
                        declaringClass = dc.toJVMTypeName,
                        name = name,
                        returnType = desc.returnType.toJVMTypeName,
                        parameterTypes = ArraySeq.from(desc.parameterTypes.iterator.map[String](_.toJVMTypeName))
                    )))

                case _ => throw IllegalArgumentException(s"Expected InvocationInstruction, but got $instruction")
            }

        def jvmToJavaTypes: CallSiteSerialized =
            this.copy(declaredTarget = declaredTarget.map(_.jvmToJavaTypes))

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
                    val parameterTypes = scala.collection.compat.immutable.ArraySeq.from(callingMethod.parameterTypes.map(FieldType.apply));
                    val md = MethodDescriptor(parameterTypes, returnType);
                    val method = classFile.findMethod(callingMethod.name, md).getOrElse(throw IllegalArgumentException(s"method ${callingMethod.declaringClass}.$callingMethod not found"))
                    val code = method.body.getOrElse(throw IllegalArgumentException(s"No body for method ${callingMethod.declaringClass}.$callingMethod"))
                    val pcAndInstruction = code.iterator.find(instruction => instruction.pc == callSite.pc).getOrElse(throw IllegalArgumentException(s"instruction not found at pc ${callSite.pc}"))
                    callSite.addDeclaredTarget(pcAndInstruction.instruction)
                } catch {
                    case exception: Throwable =>
                        System.err.println(s"Cannot find declared callsite of ${callingMethod.declaringClass}.${callingMethod}:${callSite.pc}: ${exception.getMessage()}")
                        if(!exception.isInstanceOf[IllegalArgumentException])
                            System.err.println(exception)
                        callSite
                }

            ).toMap

            this.copy(callSites = updatedCallSites)

        def jvmToJavaTypes: CallGraphSerialized =
            this.copy(
                callSites = callSites.view.mapValues(_.jvmToJavaTypes).toMap,
                methods = methods.view.mapValues(_.jvmToJavaTypes).toMap
            )

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
        def stackTraces(searchedMethod: Method): Set[List[CallSite]] = {
            val stackTracesSet = mutable.Set.empty[List[CallSite]]
            this.stackTraces(List(), searchedMethod, stackTracesSet)
            stackTracesSet.toSet
        }

        private def stackTraces(parentTrace: List[CallSite], searchedMethod: Method, stackTraces: mutable.Set[List[CallSite]]): Unit =
            for((callSite, subTree) <- callSites) {
                if(callSite.method == searchedMethod)
                    stackTraces += callSite.copy(line = -1, pc = -1, declaredTarget = None) +: parentTrace

                subTree.stackTraces(callSite +: parentTrace, searchedMethod, stackTraces)
            }


        def callersOf(callee: Method): Set[CallSite] =
            stackTraces(callee).flatMap(stackTrace => stackTrace.lift(1))

        def toReachableMethods: ReachableMethods =
            val reachableMethods = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]
            addReachableMethods(reachableMethods)
            val defaultDeclaredTarget = Method(name = "", declaringClass = "", returnType = "", parameterTypes = ArraySeq.empty)
            ReachableMethods(reachableMethods.view.map((method, callSiteMap) =>
                method ->
                    callSiteMap.view.map((callSite, targets) =>
                        new OuterCallSite(declaredTarget = callSite.declaredTarget.getOrElse(defaultDeclaredTarget), line = callSite.line, pc = Some(callSite.pc)) -> targets.toSet
                    ).toMap
                ).toMap
            )

        private def addReachableMethods(reachableMethods: mutable.Map[Method, mutable.Map[CallSite, mutable.Set[Method]]]): Unit =
            for ((callSite, subTree) <- callSites) {
                val method = callSite.method
                val callSiteMap = reachableMethods.getOrElseUpdate(callSite.method, mutable.Map.empty)
                val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
                targets ++= subTree.callSites.keySet.map(_.method)

                subTree.addReachableMethods(reachableMethods)
            }


    implicit val callTreeReads: Reads[CallTree] = (json: JsValue) =>
        callGraphSerializedReads.reads(json).map(_.deserialize)

    private def writeJson(output: Writer, json: JsValue): Unit = {
        Using(new JsonFactory().createGenerator(output)) { generator =>
            def writeNode(value: JsValue): Unit = value match {
                case JsNull => generator.writeNull()
                case JsBoolean(b) => generator.writeBoolean(b)
                case JsNumber(n) => generator.writeNumber(n.bigDecimal)
                case JsString(s) => generator.writeString(s)
                case JsArray(elements) =>
                    generator.writeStartArray()
                    elements.foreach(writeNode)
                    generator.writeEndArray()
                case JsObject(fields) =>
                    generator.writeStartObject()
                    fields.foreach { case (key, v) =>
                        generator.writeFieldName(key)
                        writeNode(v)
                    }
                    generator.writeEndObject()
            }

            writeNode(json)
        }.get
    }

}
