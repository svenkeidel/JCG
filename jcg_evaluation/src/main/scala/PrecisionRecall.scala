import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import org.apache.commons.math3.stat.descriptive.moment.Mean
import org.apache.commons.math3.stat.descriptive.rank.Percentile
import org.apache.commons.math3.stat.descriptive.summary.Sum
import play.api.libs.json.{JsObject, JsString, JsValue, Json, Reads, Writes, __}

import java.io.{File, FileInputStream}
import java.nio.file.{Path, Paths}
import java.util
import java.util.HashSet as JHashSet
import scala.collection.compat.immutable.ArraySeq
import scala.collection.immutable.IntMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.math.*
import scala.util.matching.Regex

class Classification[X](
    val actualPositive: Set[X],
    val predictedPositive: Set[X]
):
    val truePositive: Set[X] = predictedPositive.intersect(actualPositive)
    val falsePositive: Set[X] = predictedPositive -- actualPositive
    val falseNegative: Set[X] = actualPositive -- predictedPositive

    def precision: Double =
        if(predictedPositive.isEmpty)
            if(actualPositive.isEmpty) 1.0d else 0.0d
        else
            (BigDecimal(truePositive.size) / BigDecimal(predictedPositive.size)).toDouble

    def recall: Double =
        if (actualPositive.isEmpty)
            1.0d
        else
            (BigDecimal(truePositive.size) / BigDecimal(actualPositive.size)).toDouble

    val f1Score: Double =
        harmonicMean(precision,recall)

private def harmonicMean(x: Double, y: Double): Double = {
    if (x == 0 && y == 0)
        0
    else
        (2 * x * y) / (x + y)
}

class AverageingClassification[X](override val actualPositive: Set[X], override val predictedPositive: Set[X]) extends Classification[X](actualPositive, predictedPositive):
    override def precision: Double =
        if (predictedPositive.isEmpty)
            Double.NaN
        else
            (BigDecimal(truePositive.size) / BigDecimal(predictedPositive.size)).toDouble

    override def recall: Double =
        if (actualPositive.isEmpty)
            Double.NaN
        else
            (BigDecimal(truePositive.size) / BigDecimal(actualPositive.size)).toDouble


case class EdgeClassification(actualPositiveEdges: Set[Edge], predictedPositiveEdges: Set[Edge], methods: Classification[Method], computeFalsePositiveClosureSize: Boolean, computeFalseNegativeClosureSize: Boolean) extends Classification[Edge](actualPositiveEdges, predictedPositiveEdges):

    val falsePositiveClosure: Map[Method, TransitiveClosureSize] = {
        val falsePositiveGraph: Map[Method, Set[Edge]] = falsePositive.map(edge => edge.target -> Set.empty[Edge]).toMap ++ falsePositive.groupBy(edge => edge.caller)
        if(computeFalsePositiveClosureSize) purdomTransitiveClosure(falsePositiveGraph)
        else Map.empty
    }
    val falseNegativeClosure: Map[Method, TransitiveClosureSize] = {
        val falseNegativeGraph: Map[Method, Set[Edge]] = falseNegative.map(edge => edge.target -> Set.empty[Edge]).toMap ++ falseNegative.groupBy(edge => edge.caller)
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

case class AveragePerCallType(calls: Map[Method,IntMap[Call]], actualPositive: Set[Edge], predictedPositive: Set[Edge]):
    val classificationPerCallsite: Map[(Method,Int), AverageingClassification[Method]] = {
        val actualTargetsPerCallsite: Map[(Method, Int), Set[Method]] =
            actualPositive.groupBy(edge => (edge.caller, edge.line.getOrElse(-1)))
                          .view.mapValues(edges => edges.map(_.target)).toMap

        val predictedTargetsPerCallsite: Map[(Method, Int), Set[Method]] =
            predictedPositive.groupBy(edge => (edge.caller, edge.line.getOrElse(-1)))
                             .view.mapValues(edges => edges.map(_.target)).toMap

        val allCallSites: Set[(Method, Int)] = actualTargetsPerCallsite.keySet ++ predictedTargetsPerCallsite.keySet

        (for(callSite <- allCallSites.iterator)
            yield(callSite -> AverageingClassification[Method](
                actualPositive = actualTargetsPerCallsite.getOrElse(callSite, Set.empty),
                predictedPositive = predictedTargetsPerCallsite.getOrElse(callSite, Set.empty)
            ))).toMap
    }

    val precisionPerCallType: mutable.Map[CallType,mutable.ArrayBuffer[Double]] = mutable.Map.empty
    val recallPerCallType: mutable.Map[CallType,mutable.ArrayBuffer[Double]] = mutable.Map.empty
    val truePositivePerCallType: mutable.Map[CallType,mutable.ArrayBuffer[Double]] = mutable.Map.empty
    val falsePositivePerCallType: mutable.Map[CallType,mutable.ArrayBuffer[Double]] = mutable.Map.empty
    val falseNegativePerCallType: mutable.Map[CallType, mutable.ArrayBuffer[Double]] = mutable.Map.empty
    val numCallSitesPredicted: mutable.Map[CallType,Long] = mutable.Map.empty
    val numCallSitesActual: mutable.Map[CallType,Long] = mutable.Map.empty

    for(((method,line),classification) <- classificationPerCallsite) {
        val callType = calls.get(method).flatMap(callSites => callSites.get(line).map(_.callType)).getOrElse(CallType.Unclassified)
        precisionPerCallType.getOrElseUpdate(callType, new mutable.ArrayBuffer) += classification.precision
        recallPerCallType.getOrElseUpdate(callType, new mutable.ArrayBuffer) += classification.recall
        truePositivePerCallType.getOrElseUpdate(callType, new mutable.ArrayBuffer) += classification.truePositive.size
        falsePositivePerCallType.getOrElseUpdate(callType, new mutable.ArrayBuffer) += classification.falsePositive.size
        falseNegativePerCallType.getOrElseUpdate(callType, new mutable.ArrayBuffer) += classification.falseNegative.size
        numCallSitesActual += callType -> (numCallSitesActual.getOrElse(callType,0l) + (if(classification.actualPositive.isEmpty) 0l else 1l))
        numCallSitesPredicted += callType -> (numCallSitesPredicted.getOrElse(callType,0l) + (if(classification.predictedPositive.isEmpty) 0l else 1l))
    }

    def toJson: Map[String,JsValue] = {
        val callTypes = precisionPerCallType.keySet
        callTypes.toSeq.map(callType => {
            val precision = getMeanOrZero(precisionPerCallType(callType).toArray)
            val recall = getMeanOrZero(recallPerCallType(callType).toArray)
            "average-"+callType.toString.toLowerCase -> Json.obj(
                "precision" -> precision,
                "recall" -> recall,
                "f1-score" -> harmonicMean(precision, recall),
                "true_positive" -> getSumOrZero(truePositivePerCallType(callType).toArray),
                "false_positive" -> getSumOrZero(falsePositivePerCallType(callType).toArray),
                "false_negative" -> getSumOrZero(falseNegativePerCallType(callType).toArray),
                "num_callsites_actual" -> numCallSitesActual(callType),
                "num_callsites_predicted" -> numCallSitesPredicted(callType)
            )
        }).toMap
    }

    private def getMeanOrZero(values: Array[Double]): Double =
        val mean = new Mean().evaluate(values.filter(d => !d.isNaN))
        if(mean.isNaN)
            0.0d
        else
            mean

    private def getSumOrZero(values: Array[Double]): Double =
        val sum = new Sum().evaluate(values.filter(d => !d.isNaN))
        if (sum.isNaN)
            0.0d
        else
            sum


def PrecisionRecallJava(calls: Map[Method,IntMap[Call]],
                        actualCallGraph: Map[Method, Map[CallSite, Set[CallTarget]]],
                        predictedCallGraph: Map[Method, Map[CallSite, Set[CallTarget]]],
                        packageScope: Regex,
                        reachableMethodsInclude: Regex,
                        edgeInclude: Regex,
                        reachableMethodsExclude: Regex,
                        edgeExclude: Regex,
                        computeFalsePositiveClosureSize: Boolean,
                        computeFalseNegativeClosureSize: Boolean
                       ): PrecisionRecall =

    def filterMethods(callGraph: Map[Method, Map[CallSite, Set[CallTarget]]]): Set[Method] = {
        val methods = callGraph.flatMap((method,callSitesMap) => callSitesMap.values.flatten.map(_.method).toSet + method).toSet
        methods.filter(method =>
            packageScope.matches(method.declaringClass) &&
                reachableMethodsInclude.matches(method.declaringClass) &&
                !reachableMethodsExclude.matches(method.declaringClass) &&
                !internalMethod(method))
    }

    def internalMethod(method: Method): Boolean =
        (method.declaringClass.endsWith("$$Lambda") && (method.name == "get$Lambda" || method.name == "<init>"))

    def filterEdges(cg: Map[Method, Map[CallSite, Set[CallTarget]]]): Set[Edge] =
        val result = for {
            (caller, callSiteMap) <- cg;
            (callSite,targets) <- callSiteMap;
            target <- targets
            edge = Edge(caller = caller, line = Some(callSite.line), declaredTarget = callSite.declaredTarget, target = target.method)
            if(includeEdge(edge))
        } yield(removeCallerAndLineNumberOfStaticInitializers(edge))

        result.toSet

    def includeEdge(edge: Edge): Boolean = {
        val edgeString = s"${edge.caller.declaringClass} -> ${edge.target.declaringClass}";
        (packageScope.matches(edge.caller.declaringClass) || packageScope.matches(edge.target.declaringClass)) &&
        edgeInclude.matches(edgeString) &&
        !edgeExclude.matches(edgeString) &&
        !internalCall(edge)
    }

    /** detects internal calls to checkPackageAccess and loadClass that we want to exclude from */
    def internalCall(edge: Edge): Boolean = {
        // Class Loading
        (edge.target.declaringClass == "java.lang.ClassLoader" && edge.target.name == "loadClass" && edge.declaredTarget.name != "loadClass") ||
        (edge.target.declaringClass == "java.lang.ClassLoader" && edge.target.name == "checkPackageAccess" && edge.declaredTarget.name != "checkPackageAccess") ||
        // Closures
        ((edge.target.declaringClass.startsWith("java.lang.invoke.LambdaForm$MH") || edge.target.declaringClass == "java.lang.invoke.Invokers$Holder")
            && edge.target.name == "linkToTargetMethod"
            && edge.declaredTarget.name != "linkToTargetMethod"
            ) ||
        (edge.target.declaringClass == "java.lang.invoke.MethodHandleNatives" && edge.target.name == "linkCallSite" && edge.declaredTarget.name != "linkCallSite") ||
        (edge.target.declaringClass == "java.lang.invoke.MethodHandleNatives" && edge.target.name == "linkMethodHandleConstant" && edge.declaredTarget.name != "linkMethodHandleConstant") ||
        (edge.target.declaringClass == "java.lang.invoke.MethodHandleNatives" && edge.target.name == "findMethodHandleType" && edge.declaredTarget.name != "findMethodHandleType") ||
        (edge.caller.declaringClass.startsWith("java.lang.invoke.LambdaForm$DMH") && edge.target.declaringClass.endsWith("$$Lambda") && edge.target.name == "get$Lambda") ||
        (edge.caller.declaringClass.endsWith("$$Lambda") && (edge.caller.name == "get$Lambda" || edge.caller.name == "<init>")) ||
        (edge.target.declaringClass.endsWith("$$Lambda") && (edge.target.name == "get$Lambda" || edge.target.name == "<init>"))
    }


    def removeCallerAndLineNumberOfStaticInitializers(edge: Edge): Edge =
        if(edge.target.name == "<clinit>")
            edge.copy(
                caller = Method(declaringClass = "", name = "", returnType = "", parameterTypes = ArraySeq.empty),
                line = None
            )
        else
            edge

    def removeCallsToDynamicallyGeneratedClosureClasses(cg: Map[Method, Map[CallSite, Set[CallTarget]]]): Map[Method, Map[CallSite, Set[CallTarget]]] =
        var callGraph = cg
        for((caller,callSiteMap) <- cg;
            (callSite,targets) <- callSiteMap;
            closure <- targets;
            if(closure.method.declaringClass.endsWith("$$Lambda"))) {
            val closureTargets = for (closureCallSiteMap <- cg.get(closure.method).toSeq;
                                      closureTargets <- closureCallSiteMap.values;
                                      closureTarget <- closureTargets) yield (closureTarget)

            // Copy closure targets to caller
            val updatedCallSiteMap = callGraph(caller)
            val updatedTargets = updatedCallSiteMap(callSite)

            callGraph += caller -> (updatedCallSiteMap + (callSite -> (updatedTargets ++ closureTargets)))
        }

        // Remove dynamically generated closure classes
        callGraph = callGraph.filter((method,_) => !method.declaringClass.endsWith("$$Lambda"))
            .view
            .mapValues(callSiteMap =>
                callSiteMap
                    .view
                    .mapValues(targets => targets.filter(target => !target.method.declaringClass.endsWith("$$Lambda")))
                    .filter((_,targets) => targets.nonEmpty)
                    .toMap
            )
            .filter((_,callSiteMap) => callSiteMap.nonEmpty)
            .toMap

        callGraph

    val actualCallGraphWithoutClosures = removeCallsToDynamicallyGeneratedClosureClasses(actualCallGraph)

    val predictedCallGraphWithoutClosures = removeCallsToDynamicallyGeneratedClosureClasses(predictedCallGraph)

    PrecisionRecall(
        calls = calls,
        actualCallGraph = PrecisionRecallCallGraph(
            methods = filterMethods(actualCallGraphWithoutClosures),
            edges = filterEdges(actualCallGraphWithoutClosures)
        ),
        predictedCallGraph = PrecisionRecallCallGraph(
            methods = filterMethods(predictedCallGraphWithoutClosures),
            edges = filterEdges(predictedCallGraphWithoutClosures)
        ),
        computeFalsePositiveClosureSize = computeFalsePositiveClosureSize,
        computeFalseNegativeClosureSize = computeFalseNegativeClosureSize
    )

case class PrecisionRecall(
      calls: Map[Method,IntMap[Call]],
      actualCallGraph: PrecisionRecallCallGraph,
      predictedCallGraph: PrecisionRecallCallGraph,
      computeFalsePositiveClosureSize: Boolean,
      computeFalseNegativeClosureSize: Boolean
):

    val methods: Classification[Method] = Classification[Method](
        actualCallGraph.methods,
        predictedCallGraph.methods
    )


    val edges: EdgeClassification = EdgeClassification(
        actualCallGraph.edges.map(edge => edge.removeLineNumber),
        predictedCallGraph.edges.map(edge => edge.removeLineNumber),
        methods,
        computeFalsePositiveClosureSize, computeFalseNegativeClosureSize
    )

    val edgesWithCallSiteLineNumbers: EdgeClassification = EdgeClassification(
        actualCallGraph.edges,
        predictedCallGraph.edges,
        methods,
        computeFalsePositiveClosureSize, computeFalseNegativeClosureSize
    )

    val averagePerCallType: AveragePerCallType = AveragePerCallType(
        calls,
        actualCallGraph.edges,
        predictedCallGraph.edges
    )

case class PrecisionRecallCallGraph(methods: Set[Method], edges: Set[Edge])

case class Edge(caller: Method, line: Option[Int], declaredTarget: Method, target: Method):
    override def equals(obj: Any): Boolean =
        obj match
            case other: Edge => this.caller == other.caller && this.line == other.line && this.target == other.target
            case _ => false
    override def hashCode(): Int = (caller,line,target).hashCode()
    override def toString: String = s"$caller: ${line.iterator.mkString} [$declaredTarget] -> $target"
    def removeLineNumber: Edge = copy(line = None)

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
