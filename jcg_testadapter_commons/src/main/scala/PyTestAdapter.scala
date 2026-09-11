import java.io.Writer

trait PyTestAdapter extends TestAdapter {
    override val language = "python"

    override def warmup(algorithm: String, inputDirPath: String, adapterOptions: AdapterOptions): AnalysisResult = throw NotImplementedError()
    override def measureTime(algorithm: String, inputDirPath: String, adapterOptions: AdapterOptions): AnalysisResult = throw NotImplementedError()
    override def measureMemory(algorithm: String, inputDirPath: String, adapterOptions: AdapterOptions): AnalysisResult = throw NotImplementedError()
}
