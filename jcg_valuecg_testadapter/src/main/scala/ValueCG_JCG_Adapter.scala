import org.jcg.valuecgadapter.ValueCG_TestAdapterImpl

object ValueCG_JCG_Adapter extends JavaTestAdapter {
  override val frameworkName: String = "ValueCG"
  override val possibleAlgorithms: Array[String] = Array("precise", "fast")

  override def serializeCG(
    algorithm: String,
    target: String,
    output: java.io.Writer,
    adapterOptions: AdapterOptions
  ): Long = {
    new ValueCG_TestAdapterImpl().serializeCG(
      algorithm,
      target,
      output,
      adapterOptions.getString("mainClass"),
      adapterOptions.getStringArray("classPath"),
      adapterOptions.getPath("JDKPath"),
      adapterOptions.getBoolean("analyzeJDK")
    )
  }
}
