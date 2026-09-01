import play.api.libs.json.{Json, Reads, Writes, __}

import java.io.{BufferedReader, Writer}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/**
 * Representation of all Methods that are reachable in the represented call graph.
 *
 * @author Florian Kuebler
 */
case class ReachableMethods(reachableMethods: Map[Method, Map[CallSite, Set[Method]]]) {

    def writeCsv(writer: Writer): Unit = {
        writer.write("caller|line|pc|declared-target|target\n")
        for((caller,callSites) <- reachableMethods;
            (callSite,targets) <- callSites;
            target <- targets) {
            writer.write(caller.toString + "|")
            writer.write(callSite.line + "|")
            writer.write(callSite.pc.iterator.mkString + "|")
            writer.write(callSite.declaredTarget.toString + "|")
            writer.write(target.toString + "\n")
        }
    }
}

object ReachableMethods:
    def apply(callGraph: mutable.Map[Method, mutable.Map[CallSite, mutable.Set[Method]]]): ReachableMethods =
        ReachableMethods(
            callGraph.view.mapValues(callSiteMap =>
                callSiteMap.view.mapValues(targets => targets.toSet).toMap
            ).toMap
        )

    def readCsv(reader: BufferedReader): ReachableMethods = {
        // Skip header
        reader.readLine()

        val callGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[Method]]]
        while {
            val csvLine = reader.readLine()
            if(csvLine != null) {
                val Array(callerStr, lineStr, pcStr, declaredTargetStr, targetStr) = csvLine.split("\\|")
                val caller = Method.fromString(callerStr)
                val line = lineStr.toInt
                val pc = pcStr.toIntOption
                val declaredTarget = try { Method.fromString(declaredTargetStr) } catch { case _: Exception => Method(name = "", declaringClass = "", returnType = "", parameterTypes = ArraySeq.empty) }
                val target = Method.fromString(targetStr)

                val callSite = CallSite(
                    declaredTarget = declaredTarget,
                    line = line,
                    pc = pc
                )
                val callSiteMap = callGraph.getOrElseUpdate(caller, mutable.Map.empty)
                val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
                targets += target
                true
            } else {
                false
            }
        } do ()

        ReachableMethods(callGraph)
    }

given ReachableMethodsReads: Reads[ReachableMethods] = Json.reads[ReachableMethods]
given ReachableMethodsWrites: Writes[ReachableMethods] = Json.writes[ReachableMethods]

/**
 * A call site has a `declaredTarget` method, is associated with a line number (-1 if unknown)
 */
case class CallSite(declaredTarget: Method, line: Int, pc: Option[Int])

given CallSiteReads: Reads[CallSite] = Json.reads[CallSite]
given CallSiteWrites: Writes[CallSite] = Json.writes[CallSite]

/**
 * A method is represented using the `name`, the `declaringClass`, its `returnType` and its
 * `parameterTypes`.
 */
case class Method(name: String, declaringClass: String, returnType: String, parameterTypes: ArraySeq[String]) {
    override def toString: String =
        if(declaringClass.isEmpty) ""
        else s"$declaringClass: $returnType $name(${parameterTypes.mkString(",")})"


    def jvmToJavaTypes: Method = {
        Method(
            name = name,
            declaringClass = JVMType.toJavaType(declaringClass),
            returnType = JVMType.toJavaType(returnType),
            parameterTypes = parameterTypes.map(JVMType.toJavaType)
        )
    }
}

object Method {
    def fromString(string: String): Method =
        if(string.isEmpty)
            Method(declaringClass = "", name = "", returnType = "", parameterTypes = ArraySeq.empty)
        else {
            val Array(declaringClassStr, returnType, methodDescriptor) = string.split(' ')
            val declaringClass = declaringClassStr.stripSuffix(":")
            val Array(name, parameterTypes) = methodDescriptor.split('(')
            val paramTypes = parameterTypes.stripSuffix(")").split(",")
            Method(
                declaringClass = declaringClass,
                name = name,
                returnType = returnType,
                parameterTypes = if(paramTypes.length == 1 && paramTypes(0) == "") ArraySeq.empty else ArraySeq.unsafeWrapArray(paramTypes)
            )
        }
}

given MethodReads: Reads[Method] = Json.reads[Method]
given MethodWrites: Writes[Method] = Json.writes[Method]

object JVMType {
    def toLambdaNamingConvention(className: String, methodName: String, methodSignature: String, pc: Int): String =
        val method = methodName
            .replace('<', '_')
            .replace('>', '_')

        val signatureName = jvmMethodSignatureToClosureName(methodSignature)

        s"L${className}_${method}_${signatureName}_${pc}$$$$Lambda;"

    private def jvmTypeToClosureName(jvmType: String, prependToFirst: String = "", appendToFirst: String = ""): String = {
        if (jvmType.isEmpty) {
            ""
        } else if (jvmType.startsWith("L")) {
            val objectEnd = jvmType.indexOf(';')
            if (objectEnd < 0) {
                throw new IllegalArgumentException(s"Invalid JVM object type: $jvmType")
            }

            val objectType = jvmType
                .substring(1, objectEnd)
                .replace('/', '_')
                .replace("$", "__")

            val rest = jvmType.substring(objectEnd + 1)

            prependToFirst +
                objectType +
                appendToFirst +
                jvmTypeToClosureName(rest, "_")

        } else if (jvmType.startsWith("[")) {
            jvmTypeToClosureName(
                jvmType.substring(1),
                prependToFirst,
                "_array"
            )

        } else {
            val result = jvmType.charAt(0) match {
                case 'B' => "byte"
                case 'C' => "char"
                case 'D' => "double"
                case 'F' => "float"
                case 'I' => "int"
                case 'J' => "long"
                case 'S' => "short"
                case 'V' => "void"
                case 'Z' => "boolean"
                case _ => throw new IllegalArgumentException(s"Do not recognize JVM type: $jvmType")
            }

            val rest = jvmType.substring(1)

            prependToFirst +
                result +
                appendToFirst +
                jvmTypeToClosureName(rest, "_")
        }
    }

    private def jvmMethodSignatureToClosureName(methodSignature: String): String = {
        val parameterEnd = methodSignature.indexOf(')')
        if (parameterEnd < 0) {
            throw new IllegalArgumentException(s"Invalid JVM method signature: $methodSignature")
        }

        val parameters = methodSignature.substring(1, parameterEnd)
        val returnType = methodSignature.substring(parameterEnd + 1)

        jvmTypeToClosureName(returnType) + jvmTypeToClosureName(parameters, "_")
    }

    def toJavaType(jvmType: String): String =
        if(jvmType.startsWith("L") && jvmType.endsWith(";")) {
            jvmType.substring(1,jvmType.length-1).replace('/','.')
        } else if(jvmType.startsWith("[")) {
            toJavaType(jvmType.drop(1)) + "[]"
        } else {
            jvmType match {
                case "B" => "byte"
                case "C" => "char"
                case "D" => "double"
                case "F" => "float"
                case "I" => "int"
                case "J" => "long"
                case "S" => "short"
                case "V" => "void"
                case "Z" => "boolean"
                case _   => throw new IllegalArgumentException(s"Unknow type $jvmType")
            }
        }
}