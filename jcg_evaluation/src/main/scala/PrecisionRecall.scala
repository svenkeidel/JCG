import play.api.libs.json.{Json, Reads, Writes}

import java.io.{File, FileInputStream}
import java.nio.file.{Path, Paths}
import java.util
import java.util.HashSet as JHashSet
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.math.*
import scala.util.matching.Regex

case class Classification[X](
    actualPositive: Set[X],
    predictedPositive: Set[X]
):
    val truePositive: Set[X] = predictedPositive.intersect(actualPositive)
    val falsePositive: Set[X] = predictedPositive -- actualPositive
    val falseNegative: Set[X] = actualPositive -- predictedPositive

    val precision: BigDecimal =
        if(predictedPositive.isEmpty)
            if(actualPositive.isEmpty) 1 else 0
        else
            BigDecimal(truePositive.size) / BigDecimal(predictedPositive.size)

    val recall: BigDecimal =
        if (actualPositive.isEmpty)
            1
        else
            BigDecimal(truePositive.size) / BigDecimal(actualPositive.size)

    val f1Score: BigDecimal = harmonicMean(precision, recall)

    private def harmonicMean(x: BigDecimal, y: BigDecimal): BigDecimal = {
        if (x == 0 && y == 0)
            0
        else
            (2 * x * y) / (x + y)
    }

case class PrecisionRecall(
    actualCallGraph: Map[Method, Set[CallSite]],
    predictedCallGraph: Map[Method, Set[CallSite]],
    reachableMethodsInclude: Regex,
    edgeInclude: Regex
):

    val methods: Classification[Method] = Classification[Method](
        actualCallGraph.keySet.filter(method => reachableMethodsInclude.matches(method.declaringClass)),
        predictedCallGraph.keySet.filter(method => reachableMethodsInclude.matches(method.declaringClass))
    )

    val edges: Classification[Edge] = Classification[Edge](
        toEdges(actualCallGraph, withCallSiteLineNumber = false),
        toEdges(predictedCallGraph, withCallSiteLineNumber = false)
    )
    val edgesFalseNegativeBoundary: Set[Edge] = edges.falseNegative.filter(edge =>
        predictedCallGraph.contains(edge.caller)
    )

    val edgesWithCallSiteLineNumbers: Classification[Edge] = Classification[Edge](
        toEdges(actualCallGraph, withCallSiteLineNumber = true),
        toEdges(predictedCallGraph, withCallSiteLineNumber = true)
    )
    val edgesWithCallSiteLineNumbersFalseNegativeBoundary: Set[Edge] = edgesWithCallSiteLineNumbers.falseNegative.filter(edge =>
        predictedCallGraph.contains(edge.caller)
    )

    private def toEdges(cg: Map[Method, Set[CallSite]], withCallSiteLineNumber: Boolean): Set[Edge] =
        val result = for {
            (caller, callSites) <- cg;
            callSite <- callSites;
            target <- callSite.targets
            if(edgeInclude.matches(s"${caller.declaringClass} -> ${target.declaringClass}"))
            line = if(withCallSiteLineNumber) Some(callSite.line) else None
        } yield(Edge(caller = caller, line = line, target = target))
        result.toSet


case class Edge(caller: Method, line: Option[Int], target: Method):
    override def toString: String = s"$caller: ${line.iterator.mkString} -> $target"

object Edge {
    implicit val methodReads: Reads[Edge] = Json.reads[Edge]
    implicit val methodWrites: Writes[Edge] = Json.writes[Edge]
}