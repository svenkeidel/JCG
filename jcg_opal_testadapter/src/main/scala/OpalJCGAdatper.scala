import java.io.File
import java.io.Writer
import java.net.URL
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueFactory
import org.opalj.br.{ClassFile, DeclaredMethod, Type}
import org.opalj.br.analyses.{DeclaredMethods, DeclaredMethodsKey, Project, SomeProject}
import org.opalj.br.analyses.Project.JavaClassFileReader
import org.opalj.br.fpcf.FPCFAnalysisScheduler
import org.opalj.br.fpcf.analyses.pointsto.TamiFlexKey
import org.opalj.fpcf.FPCFAnalysesManagerKey
import org.opalj.br.instructions.{INVOKEDYNAMIC, MethodInvocationInstruction}
import org.opalj.tac.cg.{AllocationSiteBasedPointsToCallGraphKey, CFA_1_0_CallGraphKey, CFA_1_1_CallGraphKey, CHACallGraphKey, CTACallGraphKey, CallGraphKey, FTACallGraphKey, MTACallGraphKey, RTACallGraphKey, RemoveTacaiProvider, TypeBasedPointsToCallGraphKey, TypeIteratorKey, XTACallGraphKey}
import org.opalj.tac.fpcf.analyses.cg.{CallGraphAnalysisScheduler, TypeIterator}
import org.opalj.si.ProjectInformationKeys
import org.opalj.tac.fpcf.analyses.cg.reflection.{ReflectionRelatedCallsAnalysisScheduler, TamiFlexCallGraphAnalysisScheduler}
import org.opalj.tac.fpcf.analyses.{EagerTACAIProvider, LazyTACAIProvider}

import java.nio.file.Path
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/**
 * A [[JavaTestAdapter]] for the FPCF-based call graph analyses of OPAL.
 *
 * @author Dominik Helm
 * @author Florian Kuebler
 */
object OpalJCGAdatper extends JavaTestAdapter {

    val possibleAlgorithms: Array[String] = Array[String]("CHA", "RTA", "MTA", "CTA", "FTA", "XTA", "0-CFA", "0-1-CFA", "1-0-CFA", "1-1-CFA")

    val frameworkName: String = "Opal"

    override type Configuration = OpalConfiguration
    case class OpalConfiguration(config: Config, target: String, classPath: Array[String], jreJars: Iterable[File], callGraphKey: CallGraphKey, analyzeJDK: Boolean, var project: Project[URL] = null)

    override def configure[A](
         algorithm: String,
         target: String,
         mainClass: String,
         classPath: Array[String],
         javaVersion: Int,
         jdkPath: Path,
         analyzeJDK: Boolean)
     (actionWithConfiguration: Configuration => A): A =

        val baseConfig: Config = ConfigFactory.load().withValue(
            "org.opalj.br.reader.ClassFileReader.Invokedynamic.rewrite",
            ConfigValueFactory.fromAnyRef(true)
        )

        // configure the initial entry points
        implicit var config: Config =
            if (mainClass eq null) {
                baseConfig.withValue(
                    "org.opalj.br.analyses.cg.InitialEntryPointsKey.analysis",
                    ConfigValueFactory.fromAnyRef("org.opalj.br.analyses.cg.LibraryEntryPointsFinder")
                ).withValue(
                    "org.opalj.br.analyses.cg.InitialInstantiatedTypesKey.analysis",
                    ConfigValueFactory.fromAnyRef("org.opalj.br.analyses.cg.LibraryInstantiatedTypesFinder")
                )
            } else baseConfig.withValue(
                "org.opalj.br.analyses.cg.InitialEntryPointsKey.analysis",
                ConfigValueFactory.fromAnyRef("org.opalj.br.analyses.cg.ConfigurationEntryPointsFinder")
            ).withValue(
                "org.opalj.br.analyses.cg.InitialEntryPointsKey.entryPoints",
                ConfigValueFactory.fromIterable(
                    (
                        (baseConfig.getObjectList("org.opalj.br.analyses.cg.InitialEntryPointsKey.entryPoints").asScala :+
                            ConfigValueFactory.fromMap(Map("declaring-class" -> mainClass.replace('.', '/'), "name" -> "main").asJava))
                        ).asJava
                )
            ).withValue(
                "org.opalj.br.analyses.cg.InitialInstantiatedTypesKey.analysis",
                ConfigValueFactory.fromAnyRef("org.opalj.br.analyses.cg.ApplicationInstantiatedTypesFinder")
            )

        config = config
            .withValue("org.opalj.fpcf.analyses.AllocationSiteBasedPointsToAnalysis.mergeStringConstants", ConfigValueFactory.fromAnyRef(false))
            .withValue("org.opalj.fpcf.analyses.AllocationSiteBasedPointsToAnalysis.mergeClassConstants", ConfigValueFactory.fromAnyRef(false))

        // Fix for https://github.com/opalj/JCG/issues/16
        var modules = config.getStringList("org.opalj.tac.cg.PointsTo.modules").asScala.toSet
        modules -= "ReflectionAllocationsAnalysisScheduler"
        modules += "org.opalj.tac.fpcf.analyses.pointsto.ReflectionAllocationsAnalysisScheduler"
        config = config.withValue("org.opalj.tac.cg.PointsTo.modules",  ConfigValueFactory.fromIterable(modules.asJava))

        val jreJars = JRELocation.getAllJREJars(jdkPath).map(_.toFile)

        val callGraphKey = algorithm match {
            case "CHA" ⇒ CHACallGraphKey
            case "RTA" ⇒ RTACallGraphKey
            case "MTA" ⇒ MTACallGraphKey
            case "CTA" ⇒ CTACallGraphKey
            case "FTA" ⇒ FTACallGraphKey
            case "XTA" ⇒ XTACallGraphKey
            case "0-CFA" ⇒ TypeBasedPointsToCallGraphKey
            case "0-1-CFA" ⇒ AllocationSiteBasedPointsToCallGraphKey
            case "1-0-CFA" ⇒ CFA_1_0_CallGraphKey
            case "1-1-CFA" ⇒ CFA_1_1_CallGraphKey
        }

        actionWithConfiguration(OpalConfiguration(config = config, target = target, classPath = classPath, jreJars = jreJars, callGraphKey = callGraphKey, analyzeJDK = analyzeJDK))

    override def parseClassFilesAndGenerateIR(configuration: Configuration): Unit = {
        val cfReader = JavaClassFileReader(using theConfig = configuration.config)
        val targetClassFiles = cfReader.ClassFiles(new File(configuration.target))
        val cpClassFiles = cfReader.AllClassFiles(configuration.classPath.map(new File(_)))
        val jre = cfReader.AllClassFiles(configuration.jreJars)
        val allClassFiles = targetClassFiles ++ cpClassFiles ++ (if (configuration.analyzeJDK) jre else Seq.empty)
        val libClassFiles = if (configuration.analyzeJDK) Seq.empty else Project.JavaLibraryClassFileReader.AllClassFiles(configuration.jreJars)

        configuration.project = Project(
            allClassFiles,
            libClassFiles,
            libraryClassFilesAreInterfacesOnly = true,
            Seq.empty
        )

        configuration.project.get(FPCFAnalysesManagerKey).runAll(
            EagerTACAIProvider
        )
    }

    override type CallGraph = org.opalj.tac.cg.CallGraph

    override def computeCallGraph(configuration: Configuration): CallGraph =
        val opalCallGraph = configuration.project.get(RemoveTacaiProvider(configuration.callGraphKey))
        val typeIterator: TypeIterator = configuration.project.get(TypeIteratorKey)
        val declaredMethods: DeclaredMethods = configuration.project.get(DeclaredMethodsKey)
        opalCallGraph

    override def callGraphToJCG(configuration: Configuration, opalCallGraph: CallGraph): mutable.Map[Method, mutable.Map[CallSite, mutable.Set[Method]]] =
        val callGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]

        for {
            callerOpal <- opalCallGraph.reachableMethods()
            (pc, targets) <- opalCallGraph.calleesOf(callerOpal.method)
            tgt <- targets
            if (!callerOpal.method.name.startsWith("$string_concat") && !tgt.method.name.startsWith("$string_concat") &&
                !callerOpal.method.name.startsWith("$newInstance") && !tgt.method.name.startsWith("$newInstance"))
        } {
            val caller = opalMethodToJCGMethod(callerOpal.method)

            val target = opalMethodToJCGMethod(tgt.method)

            val defaultDeclaredTarget = Method(declaringClass = "", name = "", returnType = "", parameterTypes = ArraySeq.empty)
            val declaredTarget = try {
                tgt.method.definedMethod.body match {
                    case Some(body) => body.instructions.lift(pc) match {
                        case Some(INVOKEDYNAMIC(_, name, desc)) =>
                            Method(
                                declaringClass = "<invokedynamic>",
                                name = name,
                                returnType = convertTypeName(desc.returnType),
                                parameterTypes = ArraySeq.from(desc.parameterTypes.iterator.map[String](convertTypeName))
                            )

                        case Some(MethodInvocationInstruction(dc, _, name, desc)) =>
                            Method(
                                declaringClass = convertTypeName(dc),
                                name = name,
                                returnType = convertTypeName(desc.returnType),
                                parameterTypes = ArraySeq.from(desc.parameterTypes.iterator.map[String](convertTypeName))
                            )
                        case _ => defaultDeclaredTarget
                    }
                    case None => defaultDeclaredTarget
                }
            } catch {
                case exc: Exception => defaultDeclaredTarget
            }

            val callSite = CallSite(
                declaredTarget = declaredTarget,
                line = pc + 1,
                pc = Some(pc)
            )

            val callSiteMap = callGraph.getOrElseUpdate(caller, mutable.Map.empty)
            val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
            targets += target
        }

        callGraph

    private def opalMethodToJCGMethod(method: DeclaredMethod): Method =
        Method(
            declaringClass = convertTypeName(method.declaringClassType),
            name = method.name,
            returnType = convertTypeName(method.descriptor.returnType),
            parameterTypes = ArraySeq.from(method.descriptor.parameterTypes.iterator.map[String](tpe => convertTypeName(tpe)))
        )

    def convertTypeName(tpe: Type): String =
        JVMType.toJavaType(jvmTypeToLambdaNamingConvention(tpe.toJVMTypeName))

    def jvmTypeToLambdaNamingConvention(jvmType: String): String = {
        val LambdaName = """L(.+)[/$]([^/($]+)(\([^)]*\)[A-Z]*):(\d+)\$Lambda;""".r
        jvmType match
            case LambdaName(className, methodName, signature, pcStr) =>
                val sig = fixPackageNames(signature.replace(':', ';').replace(']','['))
                val pc = pcStr.toInt
                JVMType.toLambdaNamingConvention(className = className, methodName = methodName, methodSignature = sig, pc = pc)
            case _ => jvmType
    }

    private def fixPackageNames(methodSignature: String): String = {

        def fixObjectType(binaryName: String): String = {
            val parts = binaryName.split("\\$", -1)

            // Convention: package parts are lowercase; the first uppercase part is
            // assumed to be the top-level class name.
            val firstClassPart =
                parts.indexWhere(part => part.headOption.exists(Character.isUpperCase))

            if (firstClassPart <= 0) {
                binaryName
            } else {
                val result = new StringBuilder(binaryName.length)

                parts.indices.foreach { index =>
                    if (index > 0) {
                        // Dots before the top-level class; '$' for nested classes.
                        result.append(if (index <= firstClassPart) '/' else '$')
                    }
                    result.append(parts(index))
                }

                result.toString
            }
        }

        val result = new StringBuilder(methodSignature.length)
        var index = 0

        while (index < methodSignature.length) {
            if (methodSignature.charAt(index) == 'L') {
                val end = methodSignature.indexOf(';', index)

                if (end < 0) {
                    throw new IllegalArgumentException(
                        s"Invalid JVM method signature: $methodSignature"
                    )
                }

                result.append('L')
                result.append(fixObjectType(methodSignature.substring(index + 1, end)))
                result.append(';')

                index = end + 1
            } else {
                result.append(methodSignature.charAt(index))
                index += 1
            }
        }

        result.toString
    }
}

package org.opalj.tac.cg {

    final class RemoveTacaiProvider(callGraphKey: CallGraphKey) extends CallGraphKey:
        override def requirements(project: SomeProject): ProjectInformationKeys =
            val requirements = callGraphKey.requirements(project)
            val lazyTacaiKeys = LazyTACAIProvider.requiredProjectInformation.map(_.uniqueId).toSet
            requirements.filter(key => !lazyTacaiKeys.contains(key.uniqueId))

        override def allCallGraphAnalyses(project: SomeProject): Iterable[FPCFAnalysisScheduler] =
            val analyses: ArrayBuffer[FPCFAnalysisScheduler] = ArrayBuffer()

            analyses += CallGraphAnalysisScheduler
            analyses ++= callGraphSchedulers(project)
            analyses ++= registeredAnalyses(project)

            if (TamiFlexKey.isConfigured(project)) {
                analyses -= ReflectionRelatedCallsAnalysisScheduler
                analyses += TamiFlexCallGraphAnalysisScheduler
            }

            analyses

        override protected def registeredAnalyses(project: SomeProject): collection.Seq[FPCFAnalysisScheduler] =
            val callGraphClass = callGraphKey.getClass
            val registeredAnalysesMethod = callGraphClass.getDeclaredMethod("registeredAnalyses", project.getClass)
            registeredAnalysesMethod.setAccessible(true)
            registeredAnalysesMethod.invoke(callGraphKey, project).asInstanceOf[collection.Seq[FPCFAnalysisScheduler]]

        override def getTypeIterator(project: SomeProject): TypeIterator = callGraphKey.getTypeIterator(project)

        override protected[cg] def callGraphSchedulers(project: SomeProject): Iterable[FPCFAnalysisScheduler] =
            callGraphKey.callGraphSchedulers(project)

        override def toString: String = callGraphKey.toString
}