import java.io.File
import java.io.FileInputStream
import java.io.BufferedInputStream
import play.api.libs.json.Reads
import play.api.libs.json.Json
import play.api.libs.json.Writes
import play.api.libs.json.JsSuccess

import java.nio.file.{Files, Path, Paths}
import scala.jdk.StreamConverters.*

/**
 * Each JRE directory (`path`) is associated with the underlying java version.
 */
case class JRELocation(version: Int, path: String)

/**
 * A JVM locations specification file lists for each on the system available Java version the
 * corresponding [[JRELocation]].
 * Thus, such a file is a Json array of [[JRELocation]] entries.
 *
 * @note The JVM locations specification file should have the name jre.conf.
 *
 * @author Florian Kuebler
 */
object JRELocation {
    implicit val methodReads: Reads[JRELocation] = Json.reads[JRELocation]

    implicit val methodWrites: Writes[JRELocation] = Json.writes[JRELocation]

    /**
     * Retrieves the specified JRE directory for the given java `version`.
     */
    def jreDirectory(jreLocationsFile: File, version: Int): String = {
        Json.parse(new FileInputStream(jreLocationsFile)).validate[Array[JRELocation]] match {
            case JsSuccess(location, _) ⇒
                location.find(_.version == version).getOrElse(
                    throw new IllegalArgumentException(
                        s"java version $version not specified in jre locations"
                    )
                ).path
            case _ ⇒
                throw new IllegalArgumentException("invalid jre location specification")
        }
    }

    /**
     * From the give JRE locations specification file, this method creates a mapping from java
     * version the JRE root directory.
     */
    def mapping(jreLocationsFile: Path): Map[Int, Path] = {
        Json.parse(BufferedInputStream(FileInputStream(jreLocationsFile.toFile))).validate[Array[JRELocation]] match {
            case JsSuccess(location, _) ⇒
                location.map(jreLocation ⇒ jreLocation.version → Paths.get(jreLocation.path)).toMap
            case _ ⇒
                throw new IllegalArgumentException("invalid jre location specification")
        }
    }

    /**
     * Returns all .jar and .jmod files in the given directory and all transitive subdirectories.
     */
    def getAllJREJars(JREPath: Path): List[Path] = {
        val jars = Files.list(JREPath).filter { path ⇒
            path.getFileName.toString.endsWith(".jar") | path.getFileName.toString.endsWith(".jmod")
        }.toScala(List)
        val jarsInSubDirs = Files.list(JREPath).filter(Files.isDirectory(_)).toScala(List).flatMap(getAllJREJars)
        jars ++ jarsInSubDirs
    }
}