import play.api.libs.json.{Json, Reads, Writes}

import java.io.{File, FileInputStream}
import java.nio.file.{Path, Paths}
import java.util
import java.util.HashSet as JHashSet
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.math.*
import scala.util.matching.Regex

case class PrecisionRecall(
    actualCallGraph: Map[Method, Set[CallSite]],
    predictedCallGraph: Map[Method, Set[CallSite]],
    reachableMethodsInclude: Regex,
    edgeInclude: Regex
):
    // Methods
    val methodsActualPositive: Set[Method] = actualCallGraph.keySet.filter(method => matchesPackageFilter(method.declaringClass))
    val methodsPredictedPositive: Set[Method] = predictedCallGraph.keySet.filter(method => matchesPackageFilter(method.declaringClass))

    val methodsTruePositive: Set[Method] = methodsPredictedPositive.intersect(methodsActualPositive)
    val methodsFalsePositive: Set[Method] = methodsPredictedPositive -- methodsActualPositive
    val methodsFalseNegative: Set[Method] = methodsActualPositive -- methodsPredictedPositive

    val methodsPrecision: BigDecimal =
        if(methodsPredictedPositive.isEmpty)
            if(methodsActualPositive.isEmpty) 1 else 0
        else
            BigDecimal(methodsTruePositive.size) / BigDecimal(methodsPredictedPositive.size)

    val methodsRecall: BigDecimal =
        if(methodsActualPositive.isEmpty)
            1
        else
            BigDecimal(methodsTruePositive.size) / BigDecimal(methodsActualPositive.size)

    val methodsF1Score: BigDecimal = harmonicMean(methodsPrecision, methodsRecall)

    // Edges
    val edgesActualPositive: Set[Edge] = toEdges(actualCallGraph)
    val edgesPredictedPositive: Set[Edge] = toEdges(predictedCallGraph)

    val edgesTruePositive: Set[Edge] = edgesPredictedPositive.intersect(edgesActualPositive)
    val edgesFalsePositive: Set[Edge] = edgesPredictedPositive -- edgesActualPositive
    val edgesFalseNegative: Set[Edge] = edgesActualPositive -- edgesPredictedPositive

    val edgesPrecision: BigDecimal =
        if(edgesPredictedPositive.isEmpty)
            if(edgesActualPositive.isEmpty) 1 else 0
        else
            BigDecimal(edgesTruePositive.size) / BigDecimal(edgesPredictedPositive.size)

    val edgesRecall: BigDecimal =
        if(edgesActualPositive.isEmpty)
            1
        else
            BigDecimal(edgesTruePositive.size) / BigDecimal(edgesActualPositive.size)

    val edgesF1Score: BigDecimal = harmonicMean(edgesPrecision, edgesRecall)

    private def toEdges(cg: Map[Method, Set[CallSite]]): Set[Edge] =
        val result = for {
            (caller, callSites) <- cg;
            callSite <- callSites;
            target <- callSite.targets
            if(edgeInclude.matches(s"${caller.declaringClass} -> ${target.declaringClass}"))
        } yield(Edge(caller = caller, callerPC = callSite.pc, target = target))
        result.toSet


    private def matchesPackageFilter(declaringClass: String): Boolean =
        reachableMethodsInclude.matches(declaringClass)

    private def harmonicMean(x: BigDecimal, y: BigDecimal): BigDecimal = {
        if(x == 0 && y == 0)
            0
        else
            (2*x*y) / (x + y)
    }


case class Edge(caller: Method, callerPC: Option[Int], target: Method)

object Edge {
    implicit val methodReads: Reads[Edge] = Json.reads[Edge]

    implicit val methodWrites: Writes[Edge] = Json.writes[Edge]
}