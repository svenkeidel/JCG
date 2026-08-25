import java.io.File
import java.io.Writer
import java.net.URL
import scala.collection.JavaConverters.*
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueFactory
import org.opalj.br.DeclaredMethod
import org.opalj.br.ClassType
import org.opalj.br.analyses.DeclaredMethods
import org.opalj.br.analyses.DeclaredMethodsKey
import org.opalj.br.analyses.Project
import org.opalj.br.analyses.Project.JavaClassFileReader
import org.opalj.fpcf.PropertyStoreKey
import org.opalj.br.instructions.MethodInvocationInstruction
import org.opalj.fpcf.FinalEP
import org.opalj.fpcf.PropertyStore
import org.opalj.tac.cg.AllocationSiteBasedPointsToCallGraphKey
import org.opalj.tac.cg.CFA_1_0_CallGraphKey
import org.opalj.tac.cg.CFA_1_1_CallGraphKey
import org.opalj.tac.cg.CHACallGraphKey
import org.opalj.tac.cg.CTACallGraphKey
import org.opalj.tac.cg.FTACallGraphKey
import org.opalj.tac.cg.MTACallGraphKey
import org.opalj.tac.cg.RTACallGraphKey
import org.opalj.tac.cg.TypeBasedPointsToCallGraphKey
import org.opalj.tac.cg.TypeIteratorKey
import org.opalj.tac.cg.XTACallGraphKey
import org.opalj.tac.fpcf.analyses.cg.TypeIterator
import org.opalj.br.fpcf.properties.cg.Callees
import org.opalj.br.fpcf.properties.cg.NoCallees
import org.opalj.br.fpcf.properties.cg.NoCalleesDueToNotReachableMethod

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/**
 * A [[JavaTestAdapter]] for the FPCF-based call graph analyses of OPAL.
 *
 * @author Dominik Helm
 * @author Florian Kuebler
 */
object OpalJCGAdatper extends JavaTestAdapter {

    val possibleAlgorithms: Array[String] = Array[String]("CHA", "RTA", "MTA", "CTA", "FTA", "XTA", "0-CFA", "0-1-CFA", "1-0-CFA", "1-1-CFA")

    val frameworkName: String = "Opal"

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

        // gather the class files to be loaded
        val cfReader = JavaClassFileReader(using theConfig = config)
        val targetClassFiles = cfReader.ClassFiles(new File(inputDirPath))
        val cpClassFiles = cfReader.AllClassFiles(classPath.map(new File(_)))
        val jreJars = JRELocation.getAllJREJars(JDKPath).map(_.toFile)
        val jre = cfReader.AllClassFiles(jreJars)
        val allClassFiles = targetClassFiles ++ cpClassFiles ++ (if (analyzeJDK) jre else Seq.empty)
        val libClassFiles = if (analyzeJDK) Seq.empty else Project.JavaLibraryClassFileReader.AllClassFiles(jreJars)

        val project: Project[URL] = Project(
            allClassFiles,
            libClassFiles,
            libraryClassFilesAreInterfacesOnly = true,
            Seq.empty
        )

        /*val performInvocationsDomain = classOf[DefaultPerformInvocationsDomainWithCFGAndDefUse[_]]

        project.updateProjectInformationKeyInitializationData(AIDomainFactoryKey) {
            case None               ⇒ Set(performInvocationsDomain)
            case Some(requirements) ⇒ requirements + performInvocationsDomain
        }*/

        val before = System.nanoTime()

        implicit val ps: PropertyStore = project.get(PropertyStoreKey)

        // run call graph, along with extra analyses e.g. for reflection
        algorithm match {
            case "CHA" ⇒ project.get(CHACallGraphKey)
            case "RTA" ⇒ project.get(RTACallGraphKey)
            case "MTA" ⇒ project.get(MTACallGraphKey)
            case "CTA" ⇒ project.get(CTACallGraphKey)
            case "FTA" ⇒ project.get(FTACallGraphKey)
            case "XTA" ⇒ project.get(XTACallGraphKey)
            case "0-CFA" ⇒ project.get(TypeBasedPointsToCallGraphKey)
            case "0-1-CFA" ⇒ project.get(AllocationSiteBasedPointsToCallGraphKey)
            case "1-0-CFA" ⇒ project.get(CFA_1_0_CallGraphKey)
            case "1-1-CFA" ⇒ project.get(CFA_1_1_CallGraphKey)
        }

        implicit val typeIterator: TypeIterator = project.get(TypeIteratorKey)

        // start the computation of the call graph
        implicit val declaredMethods: DeclaredMethods = project.get(DeclaredMethodsKey)

        val after = System.nanoTime()

        val callGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]

        for {
            callerOpal <- declaredMethods.declaredMethods
            callees =  ps(callerOpal, Callees.key) match {
               case FinalEP(_, callees: Callees) => callees
               case _ => throw new RuntimeException()
            }
            callerContext <- callees.callerContexts
            (pc, targets) <- callees.callSites(callerContext)
            tgt <- targets
        } {
            val caller = opalMethodToJCGMethod(callerOpal)

            val target = opalMethodToJCGMethod(tgt.method)

            val defaultDeclaredTarget = Method(declaringClass = "", name = "", returnType = "", parameterTypes = ArraySeq.empty)
            val declaredTarget = tgt.method.definedMethod.body match {
                case Some(body) => body.instructions.lift(pc) match {
                    case Some(MethodInvocationInstruction(dc, _, name, desc)) =>
                        Method(
                            declaringClass = dc.toJava,
                            name = name,
                            returnType = desc.returnType.toJava,
                            parameterTypes = ArraySeq.from(desc.parameterTypes.iterator.map[String](_.toJava))
                        )
                    case _ => defaultDeclaredTarget
                }
                case None => defaultDeclaredTarget
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

        ReachableMethods(callGraph).writeCsv(output)

        ps.shutdown()

        after - before
    }

    private def opalMethodToJCGMethod(method: DeclaredMethod): Method =
        Method(
            declaringClass = method.declaringClassType.toJava,
            name = method.name,
            returnType = method.descriptor.returnType.toJava,
            parameterTypes = ArraySeq.from(method.descriptor.parameterTypes.iterator.map[String](_.toJava))
        )
}
