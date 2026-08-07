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

case class EdgeClassification(actualPositive: Set[Edge], predictedPositive: Set[Edge], methods: Classification[Method], computeFalsePositiveClosureSize: Boolean, computeFalseNegativeClosureSize: Boolean) extends Classification[Edge](actualPositive, predictedPositive):
    val falsePositiveGraph: Map[Method, Set[Edge]] = falsePositive.map(edge => edge.target -> Set.empty[Edge]).toMap ++ falsePositive.groupBy(edge => edge.caller)
    val falseNegativeGraph: Map[Method, Set[Edge]] = falseNegative.map(edge => edge.target -> Set.empty[Edge]).toMap ++ falseNegative.groupBy(edge => edge.caller)
    
    val falsePositiveClosure: Map[Method, TransitiveClosureSize] =
        if(computeFalsePositiveClosureSize) purdomTransitiveClosure(falsePositiveGraph)
        else Map.empty
    val falseNegativeClosure: Map[Method, TransitiveClosureSize] = {
        if(computeFalseNegativeClosureSize) purdomTransitiveClosure(falseNegativeGraph)
        else Map.empty
    }

    val falsePositiveBoundary: Map[Edge, TransitiveClosureSize] =
        falsePositive
          .filter(edge => methods.truePositive.contains(edge.caller))
          .map(edge => edge -> falsePositiveClosure.getOrElse(edge.target, TransitiveClosureSize(methods = -1, edges = -1)))
          .toMap

    val falseNegativeBoundary: Map[Edge, TransitiveClosureSize] = 
        falseNegative
          .filter(edge => methods.truePositive.contains(edge.caller))
          .map(edge => edge -> falseNegativeClosure.getOrElse(edge.target, TransitiveClosureSize(methods = -1, edges = -1)))
          .toMap

//    private def transitiveClosure(startEdge: Edge, callGraph: Map[Method, Set[Edge]], relevantMethods: Set[Method]): TransitiveClosureSize =
//        val worklist = mutable.Queue(startEdge)
//        val transitiveClosureEdges = mutable.HashSet[Edge]()
//        val transitiveClosureMethods = mutable.HashSet[Method]()
//        while (worklist.nonEmpty) {
//            val edge = worklist.removeHead()
//            if (!transitiveClosureEdges.contains(edge)) {
//                transitiveClosureEdges += edge
//
//                if (!transitiveClosureMethods.contains(edge.target) && relevantMethods.contains(edge.target)) {
//                    transitiveClosureMethods += edge.target
//
//                    worklist ++= callGraph.getOrElse(edge.target, Set.empty).diff(transitiveClosureEdges)
//                }
//            }
//        }
//        TransitiveClosureSize(methods = transitiveClosureMethods.size, edges = transitiveClosureEdges.size)


case class PrecisionRecall(
      actualCallGraph: Map[Method, Set[CallSite]],
      predictedCallGraph: Map[Method, Set[CallSite]],
      packageScope: Regex,
      reachableMethodsInclude: Regex,
      edgeInclude: Regex,
      reachableMethodsExclude: Regex,
      edgeExclude: Regex,
      computeFalsePositiveClosureSize: Boolean,
      computeFalseNegativeClosureSize: Boolean
):

    val methods: Classification[Method] = Classification[Method](
        actualCallGraph.keySet.filter(method => packageScope.matches(method.declaringClass) && reachableMethodsInclude.matches(method.declaringClass) && !reachableMethodsExclude.matches(method.declaringClass)),
        predictedCallGraph.keySet.filter(method => packageScope.matches(method.declaringClass) && reachableMethodsInclude.matches(method.declaringClass) && !reachableMethodsExclude.matches(method.declaringClass))
    )

    val edges: EdgeClassification = EdgeClassification(
        toEdges(actualCallGraph, withCallSiteLineNumber = false),
        toEdges(predictedCallGraph, withCallSiteLineNumber = false),
        methods,
        computeFalsePositiveClosureSize, computeFalseNegativeClosureSize
    )

    val edgesWithCallSiteLineNumbers: EdgeClassification = EdgeClassification(
        toEdges(actualCallGraph, withCallSiteLineNumber = true),
        toEdges(predictedCallGraph, withCallSiteLineNumber = true),
        methods,
        computeFalsePositiveClosureSize, computeFalseNegativeClosureSize
    )

    private def toEdges(cg: Map[Method, Set[CallSite]], withCallSiteLineNumber: Boolean): Set[Edge] =
        val result = for {
            (caller, callSites) <- cg;
            callSite <- callSites;
            target <- callSite.targets;
            edgeString = s"${caller.declaringClass} -> ${target.declaringClass}";
            if((packageScope.matches(caller.declaringClass) || packageScope.matches(target.declaringClass)) && edgeInclude.matches(edgeString) && !edgeExclude.matches(edgeString))
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

def purdomTransitiveClosure(callGraph: Map[Method, Set[Edge]]): Map[Method, TransitiveClosureSize] = {
    println(s"puredomTransitveClosure(callgraph(size ${callGraph.size}))")

    val start = System.nanoTime()

    val numVertices = callGraph.size
    val methodNumbering: Map[Method,Int] = callGraph.view.zipWithIndex.map{case ((method, edges), index) => (method, index)}.toMap
    val methodNumberingInverse: Map[Int, Method] = methodNumbering.view.map((method,idx) => (idx,method)).toMap

    // 1. Build adjacency list (destination, label)
    val adj = Array.fill(numVertices)(mutable.ListBuffer[(Int, Edge)]())
    for(edges <- callGraph.view.values; edge <- edges) {
        adj(methodNumbering(edge.caller)).append((methodNumbering(edge.target), edge))
    }

    // 2. Find SCCs using Tarjan's Algorithm (Labels don't change component structures)
    var index = 0
    val indices = Array.fill(numVertices)(-1)
    val lowlink = Array.fill(numVertices)(-1)
    val onStack = Array.fill(numVertices)(false)
    val stack = mutable.Stack[Int]()

    val sccMap = Array.fill(numVertices)(-1)
    val sccComponents = mutable.ListBuffer[List[Int]]()

    def strongConnect(u: Int): Unit = {
        indices(u) = index
        lowlink(u) = index
        index += 1
        stack.push(u)
        onStack(u) = true

        for ((v, _) <- adj(u)) {
            if (indices(v) == -1) {
                strongConnect(v)
                lowlink(u) = Math.min(lowlink(u), lowlink(v))
            } else if (onStack(v)) {
                lowlink(u) = Math.min(lowlink(u), indices(v))
            }
        }

        if (lowlink(u) == indices(u)) {
            val component = mutable.ListBuffer[Int]()
            var finished = false
            val sccId = sccComponents.size
            while (!finished) {
                val w = stack.pop()
                onStack(w) = false
                component.append(w)
                sccMap(w) = sccId
                if (w == u) finished = true
            }
            sccComponents.append(component.toList)
        }
    }

    for (i <- 0 until numVertices) {
        if (indices(i) == -1) strongConnect(i)
    }

    val numSccs = sccComponents.size

    // 3. Construct Condensed DAG & capture labels crossing between SCCs or inside SCCs
    val dagAdj = Array.fill(numSccs)(mutable.Map[Int, mutable.Set[Edge]]())
    val dagInDegree = Array.fill(numSccs)(0)
    val sccInternalLabels = Array.fill(numSccs)(mutable.Set[Edge]())

    for (u <- 0 until numVertices) {
        val uScc = sccMap(u)
        for ((v, label) <- adj(u)) {
            val vScc = sccMap(v)
            if (uScc == vScc) {
                sccInternalLabels(uScc).add(label)
            } else {
                val labelSet = dagAdj(uScc).getOrElseUpdate(vScc, {
                    dagInDegree(vScc) += 1
                    mutable.Set[Edge]()
                })
                labelSet.add(label)
            }
        }
    }

    // 4. Perform Topological Sort on the DAG
    val topoOrder = mutable.ListBuffer[Int]()
    val queue = mutable.Queue[Int]()
    for (i <- 0 until numSccs) {
        if (dagInDegree(i) == 0) queue.enqueue(i)
    }

    while (queue.nonEmpty) {
        val u = queue.dequeue()
        topoOrder.append(u)
        for ((v, _) <- dagAdj(u)) {
            dagInDegree(v) -= 1
            if (dagInDegree(v) == 0) queue.enqueue(v)
        }
    }

    // 5. Compute DAG reachability in reverse topological order
    // Track reachable component IDs and the compiled labels found along the paths
    val reachComponents = Array.fill(numSccs)(mutable.Set[Int]())
    val reachLabels = Array.fill(numSccs)(mutable.Set[Edge]())

    for (u <- topoOrder.reverse) {
        reachComponents(u).add(u)
        reachLabels(u) ++= sccInternalLabels(u)

        for ((v, edgeLabels) <- dagAdj(u)) {
            reachComponents(u) ++= reachComponents(v)
            reachLabels(u) ++= edgeLabels
            reachLabels(u) ++= reachLabels(v)
        }
    }

    // 6. Map aggregated data back to individual original nodes
    val results = mutable.Map[Int, TransitiveClosureSize]()

    for (u <- 0 until numVertices) {
        val uScc = sccMap(u)
        val reachableSccs = reachComponents(uScc)

        var reachableNodesCount = 0
        reachableSccs.foreach { sccId =>
            reachableNodesCount += sccComponents(sccId).size
        }

        val edgesCount = Math.max(0, reachableNodesCount - 1)

        results(u) = TransitiveClosureSize(
            methods = reachableNodesCount,
            edges = reachLabels(uScc).toSet.size
        )
    }

    val end = System.nanoTime()

    println(String.format("finish %.2fs", (end - start).toDouble * 1e-9))

    results.view.map((idx, size) => (methodNumberingInverse(idx),size)).toMap
}
