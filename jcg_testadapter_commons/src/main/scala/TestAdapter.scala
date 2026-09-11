import play.api.libs.json.{JsNumber, JsObject, JsValue, Json, Reads, Writes, __}

import java.io.Writer
import scala.concurrent.duration.*

enum AnalysisResult:
    case Success(result: JsValue)
    case Timeout(timeout: Long)
    case Exception(exception: String)

    override def toString: String = {
        this match {
            case Success(result) =>
                Json.prettyPrint(result)
            case Timeout(timeout) => s"Timeout after ${timeout.seconds.toMinutes}m"
            case Exception(exc) => s"Exception in analysis:\n${exc}"
        }
    }

object AnalysisResult:
    def callGraphComputationTime(callGraphComputation: Time): AnalysisResult =
        AnalysisResult.Success(Json.obj(
            "callGraphComputationTime" -> callGraphComputation
        ))

    def irGenerationAndCallGraphComputationTime(irGeneration: Time, callGraphComputation: Time): AnalysisResult =
        AnalysisResult.Success(Json.obj(
            "irGenerationTime" -> irGeneration,
            "callGraphComputationTime" -> callGraphComputation
        ))


given AnalysisResultWrites: Writes[AnalysisResult] = {
    case success: AnalysisResult.Success => Json.writes[AnalysisResult.Success].writes(success)
    case timeout: AnalysisResult.Timeout => Json.writes[AnalysisResult.Timeout].writes(timeout)
    case exc: AnalysisResult.Exception => Json.writes[AnalysisResult.Exception].writes(exc)
}


trait TestAdapter {
    val frameworkName: String
    val language: String
    val possibleAlgorithms: Array[String]

    def warmup(
      algorithm: String,
      inputDirPath: String,
      adapterOptions: AdapterOptions = AdapterOptions.makeEmptyOptions()
    ): AnalysisResult

    def measureTime(
      algorithm: String,
      inputDirPath: String,
      adapterOptions: AdapterOptions = AdapterOptions.makeEmptyOptions()
    ): AnalysisResult

    def measureMemory(
      algorithm: String,
      inputDirPath: String,
      adapterOptions: AdapterOptions = AdapterOptions.makeEmptyOptions()
    ): AnalysisResult

    def serializeCG(
        algorithm:      String,
        inputDirPath:   String,
        output:         Writer,
        adapterOptions: AdapterOptions = AdapterOptions.makeEmptyOptions()
    ): AnalysisResult
}
