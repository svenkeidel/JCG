import com.fasterxml.jackson.core.{JsonFactory, StreamReadConstraints, StreamWriteConstraints}

import java.io.{BufferedInputStream, File, FileInputStream, OutputStream, Writer}
import java.nio.file.*
import java.util.zip.GZIPInputStream
import play.api.libs.json.*

import scala.util.{Try, Using}

object Util {

    StreamReadConstraints.overrideDefaultStreamReadConstraints(
        StreamReadConstraints.builder()
            .maxNestingDepth(10000)
            .build()
    )

    StreamWriteConstraints.overrideDefaultStreamWriteConstraints(
        StreamWriteConstraints.builder()
            .maxNestingDepth(10000)
            .build()
    )

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
            throw java.io.IOException(s"Cannot find call graphs files $uncompressCallGraphFile or $compressCallGraphFile")
    }


    def readJSON(callGraphPath: Path): JsValue = {
        Using(if (callGraphPath.getFileName.toString.endsWith(".gz"))
                new GZIPInputStream(BufferedInputStream(FileInputStream(callGraphPath.toFile)))
            else
                BufferedInputStream(FileInputStream(callGraphPath.toFile))
        ) { input =>
            Json.parse(input)
        }.get
    }

    def readReachableMethods(callGraphPath: Path): ReachableMethods = {
        val json = readJSON(callGraphPath)
        if(callGraphPath.toString.contains("Dynamic")) {
            json.validate[DynamicJCGAdapter.CallTree].get.toReachableMethods
        } else {
            json.validate[ReachableMethods].get
        }
    }

    def readDynamicCallGraph(callGraphPath: Path): DynamicJCGAdapter.CallTree = {
        val json = readJSON(callGraphPath)
        json.validate[DynamicJCGAdapter.CallTree].get
    }

    def writeJson(outputStream: OutputStream, json: JsValue): Unit = {
        Using(new JsonFactory().createGenerator(outputStream)) { generator =>
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
