import java.io.{BufferedInputStream, File, FileInputStream}
import java.nio.file.*
import java.util.zip.GZIPInputStream
import play.api.libs.json.Json

case class JCGConfig(
    inputDir:        Path              = Paths.get("."),
    outputDir:       Path              = Paths.get("."),
    adapters:        List[TestAdapter] = List.empty,
    projectFilter:   String            = "",
    algorithmFilter: String            = "",
    timeout:         Int               = -1,
    skipAnalysis:    Boolean           = false,
    fingerprintDir:  Path              = Paths.get(""),
    compress:        Boolean           = false,
    debug:           Boolean           = false,
    parallel:        Boolean           = false,
    language:        String            = "",
    programArgs:     String            = ""
) {
    val JRE_LOCATIONS_FILE = "jre.conf"
    val SERIALIZATION_FILE_NAME = "cg.json.gz"
    val EVALUATION_RESULT_FILE_NAME = "evaluation-result.tsv"
}

object ConfigParser {
    private val ALL_ADAPTERS: List[TestAdapter] =
        EvaluationHelper.ALL_JS_ADAPTERS ++ EvaluationHelper.ALL_JAVA_ADAPTERS ++ EvaluationHelper.ALL_PY_ADAPTERS

    def parseConfig(args: Array[String]): Option[JCGConfig] = {
        import scopt.OParser
        val builder = OParser.builder[JCGConfig]
        val parser = {
            import builder._
            OParser.sequence(
                programName("Java Call Graph Tests"),
                head("JCG", "0.4.0"),
                opt[Path]('i', "input")
                    .action((dir, c) => c.copy(inputDir = dir))
                    .text("Defines the directory with the configuration files for the input projects.")
                    .required().maxOccurs(1)
                    .validate { dir =>
                        if (Files.exists(dir) && Files.isDirectory(dir)) success
                        else failure(s"Value ${dir.toAbsolutePath} must exist and must be a directory.")
                    }
                    .validate { dir =>
                        if (FileOperations.hasFilesDeep(dir.toFile, ".conf", ".js", ".py")) success
                        else failure(s"${dir.toAbsolutePath} does not contain *.conf, *.js or *.py files")
                    },
                opt[Path]('o', "output")
                    .action { (dir, c) => c.copy(outputDir = dir) }
                    .text("Defines the output directory; all files will be placed here.")
                    .required().maxOccurs(1),
                opt[String]("project-prefix")
                    .action((prefix, c) => c.copy(projectFilter = prefix))
                    .text("Defines a prefix-based filter for the input project's name. If applied only projects starting with the <prefix> will be processed.")
                    .valueName("prefix")
                    .maxOccurs(1).optional(),
                opt[String]("algorithm-prefix")
                    .action((prefix, c) => c.copy(algorithmFilter = prefix))
                    .text("Defines a prefix-based filter for the adapters call-graph algorithms names. (e.g. filter only for RTAs)")
                    .valueName("prefix")
                    .maxOccurs(1).optional(),
                opt[String]('t', name = "timeout")
                    .action((t, c) => c.copy(timeout = Integer.valueOf(t)))
                    .valueName("timeout")
                    .maxOccurs(1).optional(),
                opt[String]('a', "adapter")
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
                opt[Unit]("skip-analysis")
                    .action((_, c) => c.copy(skipAnalysis = true))
                    .text("Skips reanalysis of call graphs and instead reads call graphs from disk.")
                    .optional(),
                opt[Unit]('c', "compress")
                    .action((_, c) => c.copy(compress = true))
                    .hidden()
                    .optional(),
                opt[Unit]('d', "debug")
                    .action((_, c) => c.copy(debug = true))
                    .hidden()
                    .optional(),
                opt[Unit]('p', "parallel")
                    .action((_, c) => c.copy(parallel = true))
                    .hidden()
                    .optional(),
                opt[Path]('f', "fingerprintDir")
                    .action((dir, c) => c.copy(fingerprintDir = dir))
                    .text("provide a fingerprint for a project-specific evaluation")
                    .valueName("<path/to/dir>")
                    .optional(),
                opt[String]('l', "language")
                    .action((lang, c) => c.copy(language = lang))
                    .text("provide the language of the projects")
                    .valueName("language")
                    .required(),
                opt[String]('a', "program-args")
                    .action((args, c) => c.copy(programArgs = args))
                    .text("additional arguments passed to the call graph analyses")
                    .valueName("args"),
                checkConfig(c =>
                    // check if adapters match language
                    if (c.adapters.map(_.language.toLowerCase).forall(_ == c.language.toLowerCase)) success
                    else failure("The given adapters do not match the language of the projects.")
                )
            )
        }

        OParser.parse(parser, args, JCGConfig()) match {
            case Some(config) =>
                // If no adapter is specified, all adapters of specified language are used
                if (config.adapters.isEmpty) {
                    val adapters = ALL_ADAPTERS.filter(_.language.toLowerCase == config.language.toLowerCase)
                    Some(config.copy(adapters = adapters))
                } else {
                    Some(config)
                }
            case _ => None
        }

    }

}

object EvaluationHelper {
    val ALL_JAVA_ADAPTERS: List[JavaTestAdapter] = List(DynamicJCGAdapter, DoopAdapter, OpalJCGAdatper, SootJCGAdapter, SootUpJCGAdapter, /*SenecaJCGAdapter,*/ Tai_e_JCG_Adapter, WalaJCGAdapter)
    val ALL_JS_ADAPTERS: List[JSTestAdapter] =
        List(JSCallGraphAdapter, Code2flowCallGraphAdapter, TAJSJCGAdapter, JellyCallGraphAdapter)
    val ALL_PY_ADAPTERS: List[PyTestAdapter] =
        List(PyCGAdapter, PyanAdapter, Code2flowPyCallGraphAdapter, JarvisCallGraphAdapter)

    def getProjectsDir(projectsDir: File): File = {
        assert(projectsDir.exists(), s"${projectsDir.getPath} does not exists")
        assert(projectsDir.isDirectory, s"${projectsDir.getPath} is not a directory")
        assert(
            projectsDir.listFiles(_.getName.endsWith(".conf")).nonEmpty,
            s"${projectsDir.getPath} does not contain *.conf files"
        )
        projectsDir
    }

    def getJRELocations(jreLocationsPath: String): Map[Int, String] = {
        val jreLocationsFile = new File(jreLocationsPath)
        assert(jreLocationsFile.exists(), "please provide a jre.conf file")
        val jreLocations = JRELocation.mapping(jreLocationsFile)
        jreLocations
    }

    def getOutputDirectory(
        adapter:     TestAdapter,
        algorithm:   String,
        projectSpec: ProjectSpecification,
        resultsDir:  File
    ): File = {
        val dirName = s"${projectSpec.name}${File.separator}${adapter.frameworkName}${File.separator}$algorithm"
        new File(resultsDir, dirName)
    }

    def readReachableMethods(cgFile: File): ReachableMethods = {
        val input =
            if (cgFile.getName.endsWith(".zip") || cgFile.getName.endsWith(".gz"))
                new GZIPInputStream(BufferedInputStream(FileInputStream(cgFile)))
            else
                BufferedInputStream(FileInputStream(cgFile))

        Json.parse(input).validate[ReachableMethods].get
    }
}
