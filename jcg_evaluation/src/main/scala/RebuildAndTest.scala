/**
 * @author Michael Reif
 */
object RebuildAndTest {

    def main(args: Array[String]): Unit = {
        TestCaseExtractor.main(args)
        Commandline.main(args)
    }
}
