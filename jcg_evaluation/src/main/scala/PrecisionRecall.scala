import play.api.libs.json.{Json, Reads, Writes}

import java.io.{File, FileInputStream}
import java.nio.file.{Path, Paths}
import java.util
import java.util.HashSet as JHashSet
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.math.*
import scala.util.matching.Regex

class Classification[X](
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

case class EdgeClassification(actualPositive: Set[Edge], predictedPositive: Set[Edge], methods: Classification[Method]) extends Classification[Edge](actualPositive, predictedPositive):
//    val falsePositiveGraph: Map[Method, Set[Edge]] = falsePositive.groupBy(edge => edge.caller)
    val falseNegativeGraph: Map[Method, Set[Edge]] = falseNegative.groupBy(edge => edge.caller)

//    val falsePositiveBoundary: Map[Edge, TransitiveClosureSize] = falsePositive.filter(edge =>
//        methods.truePositive.contains(edge.caller)
//    ).map(falsePositiveEdge => falsePositiveEdge -> transitiveClosure(falsePositiveEdge, falsePositiveGraph, methods.falsePositive)).toMap

    val falseNegativeBoundary: Map[Edge, TransitiveClosureSize] = falseNegative.filter(edge =>
        methods.truePositive.contains(edge.caller)
    ).map(falseNegativeEdge => falseNegativeEdge -> transitiveClosure(falseNegativeEdge, falseNegativeGraph, methods.falseNegative)).toMap

    private def transitiveClosure(startEdge: Edge, callGraph: Map[Method, Set[Edge]], relevantMethods: Set[Method]): TransitiveClosureSize =
        val worklist = mutable.Queue(startEdge)
        val transitiveClosureEdges = mutable.HashSet[Edge]()
        val transitiveClosureMethods = mutable.HashSet[Method]()
        while (worklist.nonEmpty) {
            val edge = worklist.removeHead()
            if (!transitiveClosureEdges.contains(edge)) {
                transitiveClosureEdges += edge

                if (!transitiveClosureMethods.contains(edge.target) && relevantMethods.contains(edge.target)) {
                    transitiveClosureMethods += edge.target

                    worklist ++= callGraph.getOrElse(edge.target, Set.empty).diff(transitiveClosureEdges)
                }
            }
        }
        TransitiveClosureSize(methods = transitiveClosureMethods.size, edges = transitiveClosureEdges.size)

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

    val edges: EdgeClassification = EdgeClassification(
        toEdges(actualCallGraph, withCallSiteLineNumber = false),
        toEdges(predictedCallGraph, withCallSiteLineNumber = false),
        methods
    )

    val edgesWithCallSiteLineNumbers: EdgeClassification = EdgeClassification(
        toEdges(actualCallGraph, withCallSiteLineNumber = true),
        toEdges(predictedCallGraph, withCallSiteLineNumber = true),
        methods
    )

    private def toEdges(cg: Map[Method, Set[CallSite]], withCallSiteLineNumber: Boolean): Set[Edge] =
        val result = for {
            (caller, callSites) <- cg;
            callSite <- callSites;
            target <- callSite.targets
            if(edgeInclude.matches(s"${caller.declaringClass} -> ${target.declaringClass}"))
            line = if(withCallSiteLineNumber) Some(callSite.line) else None
        } yield(Edge(caller = caller, line = line, declaredTarget = callSite.declaredTarget, target = target))
        result.toSet





case class Edge(caller: Method, line: Option[Int], declaredTarget: Method, target: Method):
    override def equals(obj: Any): Boolean =
        obj match
            case other: Edge => this.caller == other.caller && this.line == other.line && this.target == other.target
            case _ => false
    override def hashCode(): Int = (caller,line,target).hashCode()
    override def toString: String = s"$caller: ${line.iterator.mkString} [$declaredTarget] -> $target"

object Edge {
    implicit val methodReads: Reads[Edge] = Json.reads[Edge]
    implicit val methodWrites: Writes[Edge] = Json.writes[Edge]
}

case class TransitiveClosureSize(methods: Long, edges: Long):
    override def toString: String = s"TransitiveClosureSize(methods: $methods, edges: $edges)"