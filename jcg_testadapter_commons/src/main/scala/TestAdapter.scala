import play.api.libs.json.{JsNumber, JsObject, Json, Reads, Writes, __}

import java.io.Writer
import scala.concurrent.duration.*

enum AnalysisResult:
    case Success(irGeneration: Time, callGraphComputation: Time)
    case Timeout(timeout: Long)
    case Exception(exception: String)

    override def toString: String = {
        this match {
            case Success(irGeneration, callGraphComputation) =>
                s"IR Generation Time: $irGeneration\n" +
                s"Call Graph Computation Time: $callGraphComputation"
            case Timeout(timeout) => s"Timeout after ${timeout.seconds.toMinutes}m"
            case Exception(exc) => s"Exception in analysis:\n${exc}"
        }
    }

given AnalysisResultWrites: Writes[AnalysisResult] = {
    case success: AnalysisResult.Success => Json.writes[AnalysisResult.Success].writes(success)
    case timeout: AnalysisResult.Timeout => Json.writes[AnalysisResult.Timeout].writes(timeout)
    case exc: AnalysisResult.Exception => Json.writes[AnalysisResult.Exception].writes(exc)
}


trait TestAdapter {
    val frameworkName: String
    val language: String
    val possibleAlgorithms: Array[String]

    def serializeCG(
        algorithm:      String,
        inputDirPath:   String,
        output:         Writer,
        adapterOptions: AdapterOptions = AdapterOptions.makeEmptyOptions()
    ): AnalysisResult
}
