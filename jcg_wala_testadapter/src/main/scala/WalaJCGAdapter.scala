import java.io.File
import java.io.PrintWriter
import java.io.Writer
import java.util
import java.util.stream.Collectors
import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator}
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.ibm.wala.classLoader.Language.JAVA
import com.ibm.wala.ipa.callgraph.AnalysisCacheImpl
import com.ibm.wala.ipa.callgraph.AnalysisOptions
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.cha.ClassHierarchyFactory
import com.ibm.wala.types.MethodReference
import com.ibm.wala.types.TypeReference
import com.ibm.wala.util.NullProgressMonitor
import com.ibm.wala.core.util.config.AnalysisScopeReader

import scala.collection.immutable.ArraySeq

object WalaJCGAdapter extends JavaTestAdapter {

    val possibleAlgorithms: Array[String] = Array("CHA", "RTA", "0-CFA", "1-CFA", "0-1-CFA")

    val frameworkName: String = "Wala"

    def serializeCG(
        algorithm: String,
        inputDirPath: String,
        output:         Writer,
        adapterOptions: AdapterOptions
    ): Long = {
        val mainClass = adapterOptions.getString("mainClass")
        val classPath = adapterOptions.getStringArray("classPath")
        val JDKPath = adapterOptions.getPath("JDKPath")
        val analyzeJDK = adapterOptions.getBoolean("analyzeJDK")

        val cl = Thread.currentThread.getContextClassLoader

        var cp = util.Arrays.stream(classPath).collect(Collectors.joining(File.pathSeparator))
        cp = inputDirPath + File.pathSeparator + cp

        // write wala.properties with the specified JDK and store it in the classpath
        val tmp = new File("tmp")
        tmp.mkdirs()
        val walaPropertiesFile = new File(tmp, "wala.properties")
        val pw = new PrintWriter(walaPropertiesFile)
        pw.println(s"java_runtime_dir = $JDKPath")
        pw.close()

        /*val sysloader = classOf[WalaProperties].getClassLoader.asInstanceOf[URLClassLoader]
        val sysclass = classOf[URLClassLoader]
        val m = sysclass.getDeclaredMethod("addURL", classOf[URL])
        m.setAccessible(true)
        m.invoke(sysloader, tmp.toURI.toURL)*/

        val ex = if (analyzeJDK) {
            new File(cl.getResource("no-exclusions.txt").getFile)
        } else {
            // TODO exclude more of the jdk
            new File(cl.getResource("Java60RegressionExclusions.txt").getFile)
        }

        val scope = AnalysisScopeReader.instance.makeJavaBinaryAnalysisScope(cp, ex)

        // we do not need the wala.properties anymore!
        walaPropertiesFile.delete()
        tmp.delete()

        val classHierarchy = ClassHierarchyFactory.make(scope)

        val entrypoints =
            if (mainClass == null) {
                new AllSubtypesOfApplicationEntrypoints(scope, classHierarchy)
            } else {
                val mainClassWala = "L" + mainClass.replace(".", "/")
                Util.makeMainEntrypoints(scope, classHierarchy, mainClassWala)
            }

        val options = new AnalysisOptions(scope, entrypoints)
        options.setReflectionOptions(AnalysisOptions.ReflectionOptions.FULL)

        val cache = new AnalysisCacheImpl

        val before = System.nanoTime
        val walaCallGraph =
            if (algorithm.contains("0-CFA")) {
                val ncfaBuilder = Util.makeZeroCFABuilder(JAVA, options, cache, classHierarchy)
                ncfaBuilder.makeCallGraph(options)
            } else if (algorithm.contains("0-1-CFA")) {
                val cfaBuilder = Util.makeZeroOneCFABuilder(JAVA, options, cache, classHierarchy)
                cfaBuilder.makeCallGraph(options)
            } else if (algorithm.contains("1-CFA")) {
                val cfaBuilder = Util.makeNCFABuilder(1, JAVA, options, cache, classHierarchy)
                cfaBuilder.makeCallGraph(options)
            } else if (algorithm.contains("RTA")) {
                val rtaBuilder = Util.makeRTABuilder(options, cache, classHierarchy)
                rtaBuilder.makeCallGraph(options, new NullProgressMonitor)
            } else if (algorithm.contains("CHA")) {
                import com.ibm.wala.ipa.callgraph.cha.CHACallGraph
                val CG = new CHACallGraph(classHierarchy)
                CG.init(entrypoints)
                CG
            } else throw new IllegalArgumentException
        val after = System.nanoTime

        val jcgCallGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]

        for(callerWala <- walaCallGraph.asScala;
            caller = walaMethodToJCGMethod(callerWala.getMethod.getReference);
            callSiteWala <- callerWala.iterateCallSites().asScala;
            targetWala <- walaCallGraph.getPossibleTargets(callerWala, callSiteWala).asScala) {

            val declaredTarget = walaMethodToJCGMethod(callSiteWala.getDeclaredTarget)

            val pc = callSiteWala.getProgramCounter

            val line = pc+1
//                try {
//                    callerWala.getMethod.getLineNumber(pc)
//                } catch {
//                    case _: ArrayIndexOutOfBoundsException ⇒ -1
//                }

            val callSite = CallSite(declaredTarget = declaredTarget, line = line, pc = Some(pc))

            val target = walaMethodToJCGMethod(targetWala.getMethod.getReference)

            val callSiteMap = jcgCallGraph.getOrElseUpdate(caller, mutable.Map())
            val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
            targets += target
        }

        ReachableMethods(jcgCallGraph).writeCsv(output)

        after - before
    }

    private def walaMethodToJCGMethod(method: MethodReference): Method = {
        val name = method.getName.toString
        val declaringClass = toJavaString(method.getDeclaringClass)
        val returnType = toJavaString(method.getReturnType)
        val indexes = 0 until method.getNumberOfParameters
        val params = indexes.map(i ⇒ toJavaString(method.getParameterType(i)))

        Method(name = name, declaringClass = declaringClass, returnType = returnType, parameterTypes = ArraySeq.from(params))

    }

    private def toJavaString(typeReference: TypeReference): String =
        if (typeReference.isClassType) {
            typeReference.getName.toString.drop(1).replace('/', '.')
        } else if(typeReference.isArrayType) {
            toJavaString(typeReference.getArrayElementType) + "[]"
        } else {
            JVMType.toJavaType(typeReference.getName.toString)
        }
}
