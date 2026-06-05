import java.io.{BufferedInputStream, FileInputStream, Writer}
import java.nio.file.{Files, Paths}
import java.util.zip.GZIPInputStream
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Using
import play.api.libs.json.{JsResult, JsValue, Json, Reads, __}
import play.api.libs.functional.syntax.*

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

        val javaPath = JDKPath.getParent.toAbsolutePath.toString + "/bin/java"
        val agentPath = Paths.get("jcg_dynamic_testadapter", "src", "main", "resources", "DynamicCG.so")

        val callGraphPath = Files.createTempFile("callgraph", ".json.gz")

        try {
            val agentArgs = Array(callGraphPath.toString).mkString(",")
            classPath :+= inputDirPath

            val reachableMethods = mutable.Set[Method]()
            val edges = mutable.Map[Method, mutable.Map[(Int, Int), mutable.Set[Method]]]()

            var args = List(javaPath)
            args :+= s"-Xmx${Runtime.getRuntime.maxMemory()}"
            args ++= jvmArgs
            args :+= s"-agentpath:${agentPath.toAbsolutePath}=$agentArgs"
            args ++= List("-cp", classPath.mkString(":"))
            args :+= mainClass
            args ++= programArgs

            println(args.mkString(" "))

            val before = System.nanoTime
            val processBuilder = new ProcessBuilder(args.asJava).inheritIO()

            val usrLib = Paths.get("/usr/lib/x86_64-linux-gnu")
            val libBoostPath = Files.walk(usrLib).filter(lib => lib.getFileName.toString.startsWith("libboost_iostreams")).findFirst().toScala

            if(libBoostPath.isEmpty)
                throw java.io.IOException("Cannot find boost library path")
            processBuilder.environment().put("LD_LIBRARY_PATH", libBoostPath.get.getParent.toString)

            processBuilder.start().waitFor()
            val after = System.nanoTime

            val callGraphJSON = Using(GZIPInputStream(BufferedInputStream(FileInputStream(callGraphPath.toFile)))) {
                input => output.write(String(input.readAllBytes()))
            }

            after - before
        } finally {
            Files.delete(callGraphPath)
        }
    }

    private case class CallSiteSerialized(method: String, line: Int, pc: Int):
        def deserialize(methods: Map[String,Method]): CallSite =
            CallSite(method = methods(method), line = line, pc = pc)

    private case class CallTreeSerialized(callTree: Map[String, CallTreeSerialized]):
        def deserialize(callSites: Map[String,CallSite]): CallTree =
            CallTree(callTree.map((callSite,subTree) => (callSites(callSite), subTree.deserialize(callSites))))

    private case class CallGraphSerialized(callTree: CallTreeSerialized, callSites: Map[String,CallSiteSerialized], methods: Map[String,Method]):
        def deserialize: CallTree =
            val deserializedCallSites = callSites.view.mapValues(_.deserialize(methods)).toMap
            callTree.deserialize(deserializedCallSites)


    private implicit val callSiteSerializedReads: Reads[CallSiteSerialized] = Json.reads[CallSiteSerialized]
    private implicit val callTreeSerializedReads: Reads[CallTreeSerialized] = (json: JsValue) =>
        implicitly[Reads[Map[String, CallTreeSerialized]]].reads(json).map(CallTreeSerialized(_))
    private implicit val callGraphSerializedReads: Reads[CallGraphSerialized] = Json.reads[CallGraphSerialized]

    case class CallSite(method: Method, line: Int, pc: Int)
    case class CallTree(callSites: Map[CallSite, CallTree]):
        def toReachableMethods: ReachableMethods =
            val reachableMethods = mutable.Map.empty[Method, mutable.Map[CallSite, Set[Method]]]
            addReachableMethods(reachableMethods)
            ReachableMethods(reachableMethods.view.map((method, callSites) =>
                ReachableMethod(method, callSites.view.map((callSite, targets) =>
                    new OuterCallSite(declaredTarget = null, line = callSite.line, pc = Some(callSite.pc), targets = targets)
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
