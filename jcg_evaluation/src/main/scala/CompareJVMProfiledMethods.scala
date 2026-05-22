import java.io.File
import java.io.FileInputStream
import play.api.libs.json.Json

import java.nio.file.{Path, Paths}
/**
 *
 * @author Michael Reif
 */
object CompareJVMProfiledMethods {

    def main(args: Array[String]): Unit = {
        var callGraphPath: Path = null
        var profile = ""

        args.sliding(2, 2).toList.collect {
            case Array("--callgraph", cg) ⇒
                assert(callGraphPath == null, "--callgraph is specified multiple times")
                callGraphPath = Paths.get(cg)
            case Array("--profile", cg) ⇒
                assert(profile.isEmpty, "--tamiflex is specified multiple times")
                profile = cg
        }

        val reachableMethods = Util.readReachableMethods(callGraphPath).toMap.keySet
        val profiledMethods = parseProfileMethods(profile)
        val numProfiledMethods = profiledMethods.size

        println(s"profiled methods: $numProfiledMethods")
        println(s"reachable methods: ${reachableMethods.size}")

        val unreachable = profiledMethods.filter { m =>
            !reachableMethods.exists(_.nameBasedEquals(m))
        }

        println(s"unreachable: ${unreachable.size}")
        println(
            unreachable.mkString("unreachable methods: \n\n","\t\n", "")
        )

        println(s"\n\n ${numProfiledMethods - unreachable.size} of $numProfiledMethods all methods are reachable")
    }

    private def parseProfileMethods(profile: String): List[Method] = {
        var data : List[Method] = List.empty
        val bufferedSource = scala.io.Source.fromFile(profile)
        for (line <- bufferedSource.getLines) {
            val cols = line.split("\t").map(_.trim)
            val className= cols(0)
            val methodName = cols(1)
            val method = Method(methodName, className, "", List.empty)
            data = data :+ method
        }
        bufferedSource.close
        data
    }

}