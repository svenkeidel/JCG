import java.io.{BufferedInputStream, File, FileInputStream}
import java.nio.file.*
import java.util.zip.GZIPInputStream
import play.api.libs.json.{JsValue, Json}

object Util {

    def getProjectsDir(projectsDir: File): File = {
        assert(projectsDir.exists(), s"${projectsDir.getPath} does not exists")
        assert(projectsDir.isDirectory, s"${projectsDir.getPath} is not a directory")
        assert(
            projectsDir.listFiles(_.getName.endsWith(".conf")).nonEmpty,
            s"${projectsDir.getPath} does not contain *.conf files"
        )
        projectsDir
    }

    def getJRELocations(config: CommandlineOptions): Map[Int, Path] = {
        val jreLocationsFile = Paths.get(config.JRE_LOCATIONS_FILE)
        assert(Files.exists(jreLocationsFile), "please provide a jre.conf file")
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

    def findCallGraphFile(callGraphDirectory: Path, testCase: String): Path = {
        val uncompressCallGraphFile = callGraphDirectory.resolve(s"$testCase-callgraph.json")
        val compressCallGraphFile = callGraphDirectory.resolve(s"$testCase-callgraph.json.gz")
        if(Files.exists(uncompressCallGraphFile))
            uncompressCallGraphFile
        else if(Files.exists(compressCallGraphFile))
            compressCallGraphFile
        else
            throw IllegalStateException(s"Cannot find call graphs files $uncompressCallGraphFile or $compressCallGraphFile")
    }


    def readJSON(callGraphPath: Path): JsValue = {
        val input =
            if (callGraphPath.getFileName.toString.endsWith(".gz"))
                new GZIPInputStream(BufferedInputStream(FileInputStream(callGraphPath.toFile)))
            else
                BufferedInputStream(FileInputStream(callGraphPath.toFile))

        Json.parse(input)
    }

    def readReachableMethods(callGraphPath: Path): ReachableMethods = {
        val json = readJSON(callGraphPath)
        json.validate[ReachableMethods].get
    }
}
