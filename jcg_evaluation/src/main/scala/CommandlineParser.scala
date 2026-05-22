import Action.PrecisionRecall

import java.nio.file.{Files, Path, Paths}
import scopt.OParser

import scala.util.matching.compat.Regex

enum Action:
    case Analyze
    case Assess
    case Size
    case PrecisionRecall

case class CommandlineOptions(
                                 action:          Action            = Action.Analyze,
                                 projectsDir:     Path              = Paths.get("."),
                                 callGraphsDir:   Path              = Paths.get("."),
                                 adapters:        List[TestAdapter] = List.empty,
                                 projectFilter:   String            = "",
                                 algorithmFilter: String            = "",
                                 timeout:         Int               = -1,
                                 compress:        Boolean           = false,
                                 debug:           Boolean           = false,
                                 parallel:        Boolean           = false,
                                 language:        String            = "",
                                 analyzeJdk:      Boolean           = false,
                                 analysesArgs:    String            = "",

                                 truthCallGraphsDirectory: String   = "",
                                 reachableMethodsInclude:   Regex   = Regex(".*"),
                                 edgesInclude:              Regex   = Regex(".* -> .*")
) {
    val JRE_LOCATIONS_FILE = "jre.conf"
    val SERIALIZATION_FILE_NAME = "cg.json.gz"
    val EVALUATION_RESULT_FILE_NAME = "evaluation-result.tsv"
}

object CommandlineParser {
    val ALL_JAVA_ADAPTERS: List[JavaTestAdapter] = List(DynamicJCGAdapter, DoopAdapter, OpalJCGAdatper, SootJCGAdapter, SootUpJCGAdapter, /*SenecaJCGAdapter,*/ Tai_e_JCG_Adapter, WalaJCGAdapter)
    val ALL_JS_ADAPTERS: List[JSTestAdapter] = List(JSCallGraphAdapter, Code2flowCallGraphAdapter, TAJSJCGAdapter, JellyCallGraphAdapter)
    val ALL_PY_ADAPTERS: List[PyTestAdapter] = List(PyCGAdapter, PyanAdapter, Code2flowPyCallGraphAdapter, JarvisCallGraphAdapter)
    private val ALL_ADAPTERS: List[TestAdapter] = ALL_JS_ADAPTERS ++ ALL_JAVA_ADAPTERS ++ ALL_PY_ADAPTERS

    val builder = OParser.builder[CommandlineOptions]
    val parser = {
        import builder._
        OParser.sequence(
            programName("Java Call Graph Tests"),
            head("JCG", "0.4.0"),

            opt[Path]("projects-directory")
                .action((dir, c) => c.copy(projectsDir = dir))
                .text("Defines the directory with the configuration files of the projects.")
                .required().maxOccurs(1)
                .validate { dir =>
                    if (Files.exists(dir) && Files.isDirectory(dir)) success
                    else failure(s"Value ${dir.toAbsolutePath} must exist and must be a directory.")
                }
                .validate { dir =>
                    if (FileOperations.hasFilesDeep(dir.toFile, ".conf", ".js", ".py")) success
                    else failure(s"${dir.toAbsolutePath} does not contain *.conf, *.js or *.py files")
                },
            opt[String]("project-prefix")
                .action((prefix, c) => c.copy(projectFilter = prefix))
                .text("Defines a prefix-based filter for the input project's name. If applied only projects starting with the <prefix> will be processed.")
                .valueName("prefix")
                .maxOccurs(1).optional(),

            opt[Path]("call-graphs-directory")
                .action { (dir, c) => c.copy(callGraphsDir = dir) }
                .text("Defines the directory where call graphs are written to or read from.")
                .required().maxOccurs(1),

            opt[String]("algorithm-prefix")
                .action((prefix, c) => c.copy(algorithmFilter = prefix))
                .text("Defines a prefix-based filter for the adapters call-graph algorithms names. (e.g. filter only for RTAs)")
                .valueName("prefix")
                .maxOccurs(1).optional(),
            opt[String]("adapter")
                .action { (adapterName, c) =>
                    val adapter = ALL_ADAPTERS.find(_.frameworkName.toLowerCase == adapterName.toLowerCase)
                    if (adapter.isEmpty) failure("The given <adapter> is not yet registered as valid adapter.")
                    val newAdapters = c.adapters.::(adapter.get)
                    c.copy(adapters = newAdapters)
                }
                .text("Run the pipeline for a selecton of adapters. (e.g., the <OPAL> to run the OPAL's algorithms)")
                .valueName("adapter")
                .optional()
                .unbounded(),

            opt[String]("language")
                .action((lang, c) => c.copy(language = lang))
                .text("provide the language of the projects")
                .valueName("language")
                .required(),

            opt[Unit]("debug")
                .action((_, c) => c.copy(debug = true))
                .hidden()
                .optional(),

            cmd("analyze")
                .action((_,c) => c.copy(action = Action.Analyze))
                .text("run call graph analyses on projects")
                .children(
                    opt[String]("analyses-args")
                        .action((args, c) => c.copy(analysesArgs = args))
                        .text("additional arguments passed to the call graph analyses")
                        .valueName("args"),
                    opt[String]("timeout")
                        .action((t, c) => c.copy(timeout = Integer.valueOf(t)))
                        .valueName("timeout")
                        .maxOccurs(1).optional(),
                    opt[Unit]("compress")
                        .action((_, c) => c.copy(compress = true))
                        .text("Compress call graphs")
                        .optional(),
                    opt[Unit]("parallel")
                        .action((_, c) => c.copy(parallel = true))
                        .hidden()
                        .optional(),
                    opt[Unit]("analyze-jdk")
                        .action((_,c) => c.copy(analyzeJdk = true))
                ),

            cmd("assess")
                .action((_,c) => c.copy(action = Action.Assess))
                .text("Assess soundness and precision of call graph analyses"),

            cmd("size")
                .action((_,c) => c.copy(action = Action.Size))
                .text("Computes the number of reachable methods in a call graph"),

            cmd("precision-recall")
                .action((_,c) => c.copy(action = Action.PrecisionRecall))
                .text("Computes precision and recall with respect to an expected call graph.")
                .children(
                    opt[String]("truth-call-graphs-directory")
                        .action((truthCallGraphsDirectory, c) => c.copy(truthCallGraphsDirectory = truthCallGraphsDirectory))
                        .text("Name of the subdirectory within \"call-graph-directory\" that contain the call graphs " +
                            "that serve as the truth in precision/recall computations")
                        .required(),
                    opt[String]("reachable-methods-include")
                        .action((reachableMethodsInclude, c) => c.copy(reachableMethodsInclude = Regex(reachableMethodsInclude)))
                        .text("Regular expression that filters the reachable methods before measuring precision and recall. A reachable method is included if it matches the regular expression.")
                        .valueName("regex")
                        .maxOccurs(1)
                        .optional(),
                    opt[String]("edge-include")
                        .action((edgeInclude, c) => c.copy(edgesInclude = Regex(edgeInclude)))
                        .text("Regular expression that filters the edges before measuring edge precision and edge recall. An edge \"CallerMethod -> TargetMethod\" is included if it matches the regular expression.")
                        .valueName("regex")
                        .maxOccurs(1)
                        .optional()
                ),

            checkConfig(c =>
                // check if adapters match language
                if (c.adapters.map(_.language.toLowerCase).forall(_ == c.language.toLowerCase)) success
                else failure("The given adapters do not match the language of the projects.")
            )
        )
    }

    def parseConfig(args: Array[String]): CommandlineOptions = {
        OParser.parse(parser, args, CommandlineOptions()) match {
            case Some(config) =>
                // If no adapter is specified, all adapters of specified language are used
                if (config.adapters.isEmpty) {
                    val adapters = ALL_ADAPTERS.filter(_.language.toLowerCase == config.language.toLowerCase)
                    config.copy(adapters = adapters)
                } else {
                    config
                }
            case _ => throw IllegalArgumentException(s"Cannot parse commandline options: ${args.mkString(" ")}")
        }

    }

}