import ReachableMethods.readCsv
import play.api.libs.json.{Json, Reads, Writes, __}

import java.io.{BufferedReader, InputStream, Writer}
import java.nio.file.Path
import java.util.jar.JarFile
import scala.collection.immutable.{ArraySeq, IntMap, StringOps, StringView}
import scala.collection.{View, mutable}
import scala.util.Using
import scala.jdk.StreamConverters.*
import scala.jdk.CollectionConverters.*
import org.objectweb.asm.{ClassReader, Handle, MethodVisitor, Opcodes, Type}
import org.objectweb.asm.tree.*

import java.util

case class CallTarget(method: Method, info: String)
given InfoReads: Reads[CallTarget] = Json.reads[CallTarget]
given InfoWrites: Writes[CallTarget] = Json.writes[CallTarget]

object CallTarget:
    def apply(method:Method): CallTarget = CallTarget(method = method, info = "")

/**
 * Representation of all Methods that are reachable in the represented call graph.
 *
 * @author Florian Kuebler
 */
case class ReachableMethods(reachableMethods: Map[Method, Map[CallSite, Set[CallTarget]]]) {

    def writeCsv(writer: Writer): Unit = {
        writer.write("caller|line|pc|call-type|declared-target|target|info\n")
        for((caller,callSites) <- reachableMethods.toIndexedSeq.sortBy(_._1);
            (callSite,targets) <- callSites.toIndexedSeq.sortBy(_._1);
            CallTarget(target,info) <- targets) {
            writer.write(caller.toString + "|")
            writer.write(callSite.line + "|")
            writer.write(callSite.pc.iterator.mkString + "|")
            writer.write(callSite.callType.callType + "|")
            writer.write(callSite.declaredTarget.toString + "|")
            writer.write(target.toString + "|")
            writer.write(info + "\n")
        }
    }

    def methods: Iterator[Method] = {
        val result: mutable.Set[Method] = mutable.Set.empty
        for((caller,callSiteMap) <- reachableMethods; (callSite,targets) <- callSiteMap; CallTarget(target,_) <- targets) {
            result += caller
            result += target
        }
        result.iterator
    }


    def edges: Iterator[(Method, CallSite, Method)] =
        for((caller,callSiteMap) <- reachableMethods.iterator; (callSite,targets) <- callSiteMap; CallTarget(target,_) <- targets) yield (caller, callSite, target)
}

object ReachableMethods:
    def apply(callGraph: mutable.Map[Method, mutable.Map[CallSite, mutable.Set[CallTarget]]]): ReachableMethods =
        ReachableMethods(
            callGraph.view.mapValues(callSiteMap =>
                callSiteMap.view.mapValues(targets => targets.toSet).toMap
            ).toMap
        )

    def readCsv(reader: BufferedReader): ReachableMethods = {
        val callGraph = mutable.Map.empty[Method, mutable.Map[CallSite, mutable.Set[CallTarget]]]

        // Skip header
        val headerLine = reader.readLine()
        if(headerLine != null) {
            val header = headerLine.split("\\|")

            while {
                val csvLine = reader.readLine()
                if (csvLine != null) {
                    val csvMap = header.zip(csvLine.split("\\|")).toMap

                    val caller = Method.fromString(csvMap("caller"))
                    val line = csvMap("line").toInt
                    val pc = csvMap("pc").toIntOption
                    val callType = CallType(csvMap.getOrElse("call-type", ""))
                    val declaredTarget = try {
                        Method.fromString(csvMap("declared-target"))
                    } catch {
                        case _: Throwable => Method(name = "", declaringClass = "", returnType = "", parameterTypes = ArraySeq.empty)
                    }
                    val target = Method.fromString(csvMap("target"))
                    val info = csvMap.getOrElse("info", "")

                    val callSite = CallSite(
                        callType,
                        declaredTarget = declaredTarget,
                        line = line,
                        pc = pc
                    )
                    val callSiteMap = callGraph.getOrElseUpdate(caller, mutable.Map.empty)
                    val targets = callSiteMap.getOrElseUpdate(callSite, mutable.Set.empty)
                    targets.add(CallTarget(target, info))
                    true
                } else {
                    false
                }
            } do ()
        }

        ReachableMethods(callGraph)
    }

given ReachableMethodsReads: Reads[ReachableMethods] = Json.reads[ReachableMethods]
given ReachableMethodsWrites: Writes[ReachableMethods] = Json.writes[ReachableMethods]

/**
 * A call site has a `declaredTarget` method, is associated with a line number (-1 if unknown)
 */
case class CallSite(callType: CallType, declaredTarget: Method, line: Int, pc: Option[Int])

given CallSiteReads: Reads[CallSite] = Json.reads[CallSite]
given CallSiteWrites: Writes[CallSite] = Json.writes[CallSite]
given CallSiteOrdering: Ordering[CallSite] = Ordering.by(callSite => (callSite.line, callSite.pc))

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
            try {
                val Array(declaringClassStr, returnType, methodDescriptor) = string.split(' ')
                val declaringClass = declaringClassStr.stripSuffix(":")
                val Array(name, parameterTypes) = methodDescriptor.split("\\(", 2)
                val paramTypes = parameterTypes.stripSuffix(")").split(",")
                Method(
                    declaringClass = declaringClass,
                    name = name,
                    returnType = returnType,
                    parameterTypes = if (paramTypes.length == 1 && paramTypes(0) == "") ArraySeq.empty else ArraySeq.unsafeWrapArray(paramTypes)
                )
            } catch {
                case throwable: Throwable => throw new RuntimeException(s"Error while parsing method signature $string", throwable)
            }
        }
}

given MethodReads: Reads[Method] = Json.reads[Method]
given MethodWrites: Writes[Method] = Json.writes[Method]
given MethodOrdering: Ordering[Method] = Ordering.by(method => method.toString)

case class CallType(callType: String):
    override def toString: String = callType

object CallType {
    val InvokeVirtual = new CallType("invokevirtual")
    val InvokeStatic = new CallType("invokestatic")
    val InvokeInterface = new CallType("invokeinterface")
    val InvokeSpecial = new CallType("invokespecial")
    val InvokeDynamic = new CallType("invokedynamic")
    val Unclassified = new CallType("unclassified")

    def apply(callType: String): CallType =
        callType match {
            case "invokevirtual" => InvokeVirtual
            case "invokestatic" => InvokeStatic
            case "invokeinterface" => InvokeInterface
            case "invokespecial" => InvokeSpecial
            case "invokedynamic" => InvokeDynamic
            case _ => Unclassified
        };
}
given CallTypeReads: Reads[CallType] = Json.reads[CallType]
given CallTypeWrites: Writes[CallType] = Json.writes[CallType]
given CallTypeOrdering: Ordering[CallType] = Ordering.by(method => method.callType)


case class Call(callType: CallType, declaredTarget: Method)

object ParseCalls {
    def apply(jars: Iterable[Path]): Map[Method, IntMap[Call]] =
        val calls: mutable.Map[Method, IntMap[Call]] = mutable.Map.empty
        for(jar <- jars) {
            Using(JarFile(jar.toFile)) { jarFile =>
                for(classFile <- jarFile.stream().toScala(Iterator);
                    if(classFile.getName.endsWith(".class"))) {
                    Using(jarFile.getInputStream(classFile)) { classFileInputStream =>
                        val classReader = OffsetClassReader(classFileInputStream);
                        val classNode = OffsetClassNode(classReader);
                        classReader.accept(classNode, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);
                        for(methodNode <- classNode.methods.asScala) {
                            val caller = Method(
                                declaringClass = Type.getObjectType(classNode.name).getClassName,
                                name = methodNode.name,
                                returnType = Type.getReturnType(methodNode.desc).getClassName,
                                parameterTypes = ArraySeq.unsafeWrapArray(Type.getArgumentTypes(methodNode.desc).map(_.getClassName))
                            )

                            import CallType.*

                            for(instruction <- methodNode.instructions.asScala) {
                                instruction match {
                                    case invoke: MethodInsnNode =>
                                        val declaredTarget = Method(
                                            declaringClass = Type.getObjectType(invoke.owner).getClassName,
                                            name = invoke.name,
                                            returnType = Type.getReturnType(invoke.desc).getClassName,
                                            parameterTypes = ArraySeq.unsafeWrapArray(Type.getArgumentTypes(invoke.desc).map(_.getClassName))
                                        )
                                        val pc = classNode.offsetOf(instruction)
                                        val line = pc + 1
                                        val call = invoke.getOpcode match {
                                            case Opcodes.INVOKESTATIC => Call(InvokeStatic,declaredTarget)
                                            case Opcodes.INVOKEVIRTUAL => Call(InvokeVirtual,declaredTarget)
                                            case Opcodes.INVOKEINTERFACE => Call(InvokeInterface,declaredTarget)
                                            case Opcodes.INVOKESPECIAL => Call(InvokeSpecial,declaredTarget)
                                            case opcode => throw IllegalArgumentException(s"Unexpected Opcode: $opcode")
                                        }

                                        val callSites = calls.getOrElseUpdate(caller, IntMap.empty)
                                        calls += (caller -> (callSites + (line -> call)))

                                    case invoke: InvokeDynamicInsnNode =>
                                        val declaredTarget = Method(
                                            declaringClass = Type.getObjectType(invoke.bsm.getOwner).getClassName,
                                            name = invoke.bsm.getName,
                                            returnType = Type.getReturnType(invoke.bsm.getDesc).getClassName,
                                            parameterTypes = ArraySeq.unsafeWrapArray(Type.getArgumentTypes(invoke.bsm.getDesc).map(_.getClassName))
                                        )
                                        val pc = classNode.offsetOf(instruction)
                                        val line = pc + 1
                                        val call = Call(InvokeDynamic,declaredTarget)
                                        val callSites = calls.getOrElseUpdate(caller, IntMap.empty)
                                        calls += (caller -> (callSites + (line -> call)))

                                    case _ => {}
                                }
                            }
                        }
                    }
                }
            }
        }
        calls.toMap
}

private final class OffsetClassReader(input: InputStream)
    extends ClassReader(input):

    private var offset: Int = -1

    def currentOffset: Int = offset

    override protected def readBytecodeInstructionOffset(bytecodeOffset: Int): Unit =
        offset = bytecodeOffset


private final class OffsetClassNode(reader: OffsetClassReader)
    extends ClassNode(Opcodes.ASM9):

    private val offsets =
        new util.IdentityHashMap[AbstractInsnNode, java.lang.Integer]()

    def offsetOf(instruction: AbstractInsnNode): Int =
        if !offsets.containsKey(instruction) then
            throw new IllegalArgumentException(
                s"No bytecode offset recorded for $instruction"
            )

        offsets.get(instruction).intValue()

    override def visitMethod(
        access: Int,
        name: String,
        descriptor: String,
        signature: String,
        exceptions: Array[String]
    ): MethodVisitor = {
        val methodNode =
            super
                .visitMethod(access, name, descriptor, signature, exceptions)
                .asInstanceOf[MethodNode]

        new MethodVisitor(Opcodes.ASM9, methodNode) {

            override def visitMethodInsn(
                opcode: Int,
                owner: String,
                name: String,
                descriptor: String,
                isInterface: Boolean
            ): Unit = {
                val pc = reader.currentOffset

                super.visitMethodInsn(
                    opcode,
                    owner,
                    name,
                    descriptor,
                    isInterface
                )

                offsets.put(
                    methodNode.instructions.getLast,
                    java.lang.Integer.valueOf(pc)
                )
            }

            override def visitInvokeDynamicInsn(
               name: String,
               descriptor: String,
               bootstrapMethodHandle: Handle,
               bootstrapMethodArguments: Object*
            ): Unit = {

                val pc = reader.currentOffset

                super.visitInvokeDynamicInsn(
                    name,
                    descriptor,
                    bootstrapMethodHandle,
                    bootstrapMethodArguments *
                )

                offsets.put(
                    methodNode.instructions.getLast,
                    java.lang.Integer.valueOf(pc)
                )
            }
        }
    }

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

    def toJavaType(jvmType: String): String = {
        Type.getType(jvmType).getClassName
    }
}