import play.api.libs.json.{Json, Reads, Writes, __}

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

def PrecisionRecallJava(actualCallGraph: Map[Method, Map[CallSite, Set[Method]]],
                        predictedCallGraph: Map[Method, Map[CallSite, Set[Method]]],
                        packageScope: Regex,
                        reachableMethodsInclude: Regex,
                        edgeInclude: Regex,
                        reachableMethodsExclude: Regex,
                        edgeExclude: Regex,
                        computeFalsePositiveClosureSize: Boolean,
                        computeFalseNegativeClosureSize: Boolean
                       ): PrecisionRecall =

    def filterMethods(callGraph: Map[Method, Map[CallSite, Set[Method]]]): Set[Method] = {
        val methods = callGraph.flatMap((method,callSitesMap) => callSitesMap.values.flatten.toSet + method).toSet
        methods.filter(method =>
            packageScope.matches(method.declaringClass) &&
                reachableMethodsInclude.matches(method.declaringClass) &&
                !reachableMethodsExclude.matches(method.declaringClass) &&
                !internalMethod(method))
    }

    def internalMethod(method: Method): Boolean =
        (method.declaringClass.endsWith("$$Lambda") && (method.name == "get$Lambda" || method.name == "<init>"))

    def filterEdges(cg: Map[Method, Map[CallSite, Set[Method]]]): Set[Edge] =
        val result = for {
            (caller, callSiteMap) <- cg;
            (callSite,targets) <- callSiteMap;
            target <- targets
            edge = Edge(caller = caller, line = Some(callSite.line), declaredTarget = callSite.declaredTarget, target = target)
            if(includeEdge(edge))
        } yield(removeCallSiteInsensitiveLineNumbers(edge))

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

    def isCallSiteInsensitiveComparison(edge: Edge): Boolean = {
        // Static Initializers
        (edge.target.name == "<clinit>") ||
        // Closures
        (edge.caller.declaringClass.endsWith("$$Lambda") && edge.target.name.startsWith("lambda$"))
    }


    def removeCallSiteInsensitiveLineNumbers(edge: Edge): Edge =
        if(isCallSiteInsensitiveComparison(edge))
            edge.removeLineNumber
        else
            edge

    def removeCallsToDynamicallyGeneratedClosureClasses(cg: Map[Method, Map[CallSite, Set[Method]]]): Map[Method, Map[CallSite, Set[Method]]] =
        var callGraph = cg
        for((caller,callSiteMap) <- cg;
            (callSite,targets) <- callSiteMap;
            closure <- targets;
            if(closure.declaringClass.endsWith("$$Lambda"))) {
            val closureTargets = for (closureCallSiteMap <- cg.get(closure).toSeq;
                                      closureTargets <- closureCallSiteMap.values;
                                      closureTarget <- closureTargets) yield (closureTarget)

            // Copy closure targets to caller
            callGraph += caller -> (callSiteMap + (callSite -> ((targets - closure) ++ closureTargets)))
        }

        // Remove dynamically generated closure classes
        callGraph = callGraph.filter((method,_) => !method.declaringClass.endsWith("$$Lambda"))

        callGraph

    val actualCallGraphWithoutClosures = removeCallsToDynamicallyGeneratedClosureClasses(actualCallGraph)

    val predictedCallGraphWithoutClosures = removeCallsToDynamicallyGeneratedClosureClasses(predictedCallGraph)

    PrecisionRecall(
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
