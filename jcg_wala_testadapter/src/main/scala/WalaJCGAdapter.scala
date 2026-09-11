import com.ibm.wala.classLoader.Language.JAVA
import com.ibm.wala.ipa.callgraph.{AnalysisCacheImpl, AnalysisOptions, CGNode}
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.cha.{ClassHierarchy, ClassHierarchyFactory}
import com.ibm.wala.types.MethodReference
import com.ibm.wala.types.TypeReference
import com.ibm.wala.util.NullProgressMonitor
import com.ibm.wala.core.util.config.AnalysisScopeReader
import com.ibm.wala.ipa.callgraph.cha.CHACallGraph
import com.ibm.wala.ipa.summaries.LambdaSummaryClass
import com.ibm.wala.ssa.{SSAInvokeDynamicInstruction}

import java.io.File
import java.io.PrintWriter
import java.util
import java.util.stream.Collectors

import scala.jdk.CollectionConverters.*
import scala.collection.mutable


import java.nio.file.Path
import scala.collection.immutable.ArraySeq

object WalaJCGAdapter extends JavaTestAdapter {

    val possibleAlgorithms: Array[String] = Array("CHA", "RTA", "0-CFA", "1-CFA", "0-1-CFA")

    val frameworkName: String = "Wala"

    override type Configuration = WalaConfiguration

    case class WalaConfiguration(algorithm: String, options: AnalysisOptions, cache: AnalysisCacheImpl, classHierarchy: ClassHierarchy)

    override def configure[A](
         algorithm: String,
         target: String,
         mainClass: String,
         classPath: Array[String],
         javaVersion: Int,
         jdkPath: Path,
         analyzeJDK: Boolean)
    (actionWithConfiguration: Configuration => A): A = {

        val cl = Thread.currentThread.getContextClassLoader

        var cp = util.Arrays.stream(classPath).collect(Collectors.joining(File.pathSeparator))
        cp = target + File.pathSeparator + cp

        // write wala.properties with the specified JDK and store it in the classpath
        val tmp = new File("tmp")
        tmp.mkdirs()
        val walaPropertiesFile = new File(tmp, "wala.properties")
        val pw = new PrintWriter(walaPropertiesFile)
        pw.println(s"java_runtime_dir = $jdkPath")
        pw.close()

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

        actionWithConfiguration(WalaConfiguration(algorithm, options, cache, classHierarchy))
    }

    override def generateIR(configuration: Configuration): Unit = {
        for (clazz <- configuration.classHierarchy.iterator().asScala;
             method <- clazz.getDeclaredMethods.iterator().asScala) {
            try {
                configuration.cache.getIR(method)
            } catch {
                case (_: Throwable) =>
            }
        }
    }

    override type CallGraph = com.ibm.wala.ipa.callgraph.CallGraph

    override def computeCallGraph(configuration: Configuration): WalaJCGAdapter.CallGraph = {
        configuration.algorithm match
            case "0-CFA" =>
                val ncfaBuilder = Util.makeZeroCFABuilder(JAVA, configuration.options, configuration.cache, configuration.classHierarchy)
                ncfaBuilder.makeCallGraph(configuration.options)
            case "0-1-CFA" =>
                val cfaBuilder = Util.makeZeroOneCFABuilder(JAVA, configuration.options, configuration.cache, configuration.classHierarchy)
                cfaBuilder.makeCallGraph(configuration.options)
            case "1-CFA" =>
                val cfaBuilder = Util.makeNCFABuilder(1, JAVA, configuration.options, configuration.cache, configuration.classHierarchy)
                cfaBuilder.makeCallGraph(configuration.options)
            case "RTA" =>
                val rtaBuilder = Util.makeRTABuilder(configuration.options, configuration.cache, configuration.classHierarchy)
                rtaBuilder.makeCallGraph(configuration.options, new NullProgressMonitor)
            case "CHA" =>
                val CG = new CHACallGraph(configuration.classHierarchy)
                CG.init(configuration.options.getEntrypoints.asInstanceOf)
                CG
            case _ => throw new IllegalArgumentException
    }

    override def callGraphToJCG(configuration: WalaConfiguration, walaCallGraph: CallGraph): mutable.Map[Method, mutable.Map[CallSite, mutable.Set[Method]]] = {

        val bootstrapMethods = getBootstrapMethods(walaCallGraph)

        val jcgCallGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]

        for (callerWala <- walaCallGraph.asScala;
             caller = walaMethodToJCGMethod(walaCallGraph, bootstrapMethods, callerWala.getMethod.getReference);
             callSiteWala <- callerWala.iterateCallSites().asScala;
             targetWala <- walaCallGraph.getPossibleTargets(callerWala, callSiteWala).asScala) {

            val declaredTarget = walaMethodToJCGMethod(walaCallGraph, bootstrapMethods, callSiteWala.getDeclaredTarget)

            val pc = callSiteWala.getProgramCounter

            val line = pc + 1

            val callSite = CallSite(declaredTarget = declaredTarget, line = line, pc = Some(pc))

            val target = walaMethodToJCGMethod(walaCallGraph, bootstrapMethods, targetWala.getMethod.getReference)

            val callSiteMap = jcgCallGraph.getOrElseUpdate(caller, mutable.Map())
            val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
            targets += target
        }

        jcgCallGraph
    }

    private def walaMethodToJCGMethod(callGraph: CallGraph, bootstrapMethods: Map[(String,Int), CGNode], method: MethodReference): Method = {
        val name = method.getName.toString
        val declaringClass = toJavaString(jvmTypeToLambdaNamingConvention(callGraph, bootstrapMethods, method.getDeclaringClass))
        val returnType = toJavaString(jvmTypeToLambdaNamingConvention(callGraph, bootstrapMethods, method.getReturnType))
        val indexes = 0 until method.getNumberOfParameters
        val params = indexes.map(i ⇒ toJavaString(jvmTypeToLambdaNamingConvention(callGraph, bootstrapMethods, method.getParameterType(i))))

        Method(name = name, declaringClass = declaringClass, returnType = returnType, parameterTypes = ArraySeq.from(params))
    }

    private def getBootstrapMethods(callGraph: CallGraph): Map[(String,Int), CGNode] =
        val result = for {
            node <- callGraph.asScala
            if(node.getIR != null)
            instruction <- node.getIR.getInstructions
            if(instruction.isInstanceOf[SSAInvokeDynamicInstruction])
            boostrapMethod = instruction.asInstanceOf[SSAInvokeDynamicInstruction].getBootstrap
        } yield((node.getMethod.getDeclaringClass.getName.toString.replace('/','$').drop(1),boostrapMethod.getIndexInClassFile) -> node)
        result.toMap

    private def jvmTypeToLambdaNamingConvention(callGraph: CallGraph, bootstrapMethods: Map[(String,Int), CGNode], typeReference: com.ibm.wala.types.TypeReference): TypeReference =
        if(typeReference.getName.toString.startsWith("Lwala/lambda$")) {
            var className = typeReference.getName.toString.stripPrefix("Lwala/lambda$")
            className = className.take(className.lastIndexOf('$'))

            val boostrapMethodIndex = typeReference.getName.toString.drop(typeReference.getName.toString.lastIndexOf('$') + 1).toInt

            callGraph.getClassHierarchy.lookupClass(typeReference) match {
                case lambdaSummaryClass: LambdaSummaryClass =>
                    val invokeInstruction = lambdaSummaryClass.getClass.getDeclaredField("invoke")
                    invokeInstruction.setAccessible(true)
                    val invokationInstruction = invokeInstruction.get(lambdaSummaryClass).asInstanceOf[SSAInvokeDynamicInstruction]
                    bootstrapMethods.get((className,boostrapMethodIndex)) match {
                        case Some(node) =>
                            val method = node.getMethod
                            val converted = JVMType.toLambdaNamingConvention(
                                className = method.getDeclaringClass.getName.toString.substring(1),
                                methodName = method.getName.toString,
                                methodSignature = method.getDescriptor.toString,
                                pc = invokationInstruction.getProgramCounter
                            )
                            TypeReference.findOrCreate(typeReference.getClassLoader, converted.substring(0, converted.length - 1))

                        case None =>
                            System.err.println(s"Cannot find call graph node for $invokationInstruction")
                            typeReference
                    }
                case _ =>
                    System.err.println(s"Cannot find lambda summary class for $typeReference")
                    typeReference
            }
        } else {
            typeReference
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
