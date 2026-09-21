import lib.annotations.callgraph.DirectCall
import lib.annotations.callgraph.DirectCalls
import lib.annotations.callgraph.IndirectCall
import lib.annotations.callgraph.IndirectCalls
import org.opalj.br
import org.opalj.br.{Annotation, AnnotationValue, ArrayValue, ClassType, ClassTypes, ClassValue, ElementValuePair, IntValue, StringValue, Type, VoidType}
import org.opalj.br.analyses.SomeProject

/**
 * Utility class to handle the [[DirectCall]] and [[IndirectCall]] annotations using OPAL.
 *
 * @author Florian Kuebler
 * @author Roberts Kolosovs
 */
object AnnotationHelper {

    // ClassType of the annotations:

    val DirectCallAnnotationType =
        ClassType(classOf[DirectCall].getName.replace(".", "/"))
    val DirectCallsAnnotationType =
        ClassType(classOf[DirectCalls].getName.replace(".", "/"))
    val IndirectCallAnnotationType =
        ClassType(classOf[IndirectCall].getName.replace(".", "/"))
    val IndirectCallsAnnotationType =
        ClassType(classOf[IndirectCalls].getName.replace(".", "/"))

    /**
     * Returns all [[DirectCall]] annotations referred by this annotation.
     */
    def directCallAnnotations(annotation: Annotation): Seq[Annotation] = {
        if (annotation.annotationType == AnnotationHelper.DirectCallAnnotationType)
            Seq(annotation)
        else if (annotation.annotationType == AnnotationHelper.DirectCallsAnnotationType)
            AnnotationHelper.getAnnotations(annotation, "value")
        else
            Seq.empty
    }

    /**
     * Returns all [[IndirectCall]] annotations referred by this annotation.
     */
    def indirectCallAnnotations(annotation: Annotation): Seq[Annotation] = {
        if (annotation.annotationType == AnnotationHelper.IndirectCallAnnotationType)
            Seq(annotation)
        else if (annotation.annotationType == AnnotationHelper.IndirectCallsAnnotationType)
            AnnotationHelper.getAnnotations(annotation, "value")
        else
            Seq.empty
    }

    /**
     * Does this method has a call annotation ([[DirectCall]], [[IndirectCall]] or a
     * repeatable wrapper)?
     */
    def isAnnotatedMethod(method: br.Method): Boolean = {
        method.annotations.exists { a ⇒
            a.annotationType == DirectCallAnnotationType ||
                a.annotationType == DirectCallsAnnotationType ||
                a.annotationType == IndirectCallAnnotationType ||
                a.annotationType == IndirectCallsAnnotationType
        }
    }

    /**
     * Retrieves the annotations specified in the given `annotation` under the given `label`.
     */
    def getAnnotations(annotation: Annotation, label: String): Seq[Annotation] = {
        val avs = annotation.elementValuePairs collectFirst {
            case ElementValuePair(`label`, ArrayValue(array)) ⇒ array
        }
        avs.getOrElse(IndexedSeq.empty).map { cs ⇒ cs.asInstanceOf[AnnotationValue].annotation }
    }

    /**
     * For the given `annotation`, it retrieves the string specified as `name`.
     */
    def getName(annotation: Annotation): String = {
        val sv = annotation.elementValuePairs collectFirst {
            case ElementValuePair("name", StringValue(string)) ⇒ string
        }
        sv.get
    }

    /**
     * For the given `annotation`, it retrieves the line number specified in the annotation.
     */
    def getLineNumber(annotation: Annotation): Int = {
        val iv = annotation.elementValuePairs collectFirst {
            case ElementValuePair("line", IntValue(int)) ⇒ int
        }
        iv.getOrElse(-1)
    }

    /**
     * Retrieves the [[Type]] specified in the given `annotation` with the given `label`.
     */
    def getType(annotation: Annotation, label: String): Type = { //@DirectCall -> Type
        val cv = annotation.elementValuePairs collectFirst {
            case ElementValuePair(`label`, ClassValue(declaringType)) ⇒ declaringType
        }
        cv.getOrElse(VoidType)
    }

    /**
     * For the given `annotation`, it retrieves the return type specified in the annotation.
     */
    def getReturnType(annotation: Annotation): Type = {
        getType(annotation, "returnType")
    }

    /**
     * For the given `annotation`, it retrieves the parameterTypes specified in the annotation.
     */
    def getParameterList(annotation: Annotation): Array[Type] = { //@DirectCall -> Seq[FieldType]
        val av = annotation.elementValuePairs collectFirst {
            case ElementValuePair("parameterTypes", ArrayValue(ab)) ⇒
                ab.map(ev ⇒ ev.asInstanceOf[ClassValue].value).toArray[Type]
        }
        av.getOrElse(Array.empty[Type])
    }

    /**
     * For the given `annotation`, it retrieves the resolvedTargets specified in the annotation.
     */
    def getResolvedTargets(annotation: Annotation)(implicit p: SomeProject): List[String] = {
        val av = annotation.elementValuePairs collectFirst {
            case ElementValuePair("resolvedTargets", ArrayValue(ab)) ⇒
                ab.map(elementValue => JVMType.toJavaType(elementValue.asInstanceOf[StringValue].value))
        }

        av.getOrElse(List()).toList
    }

    /**
     * For the given `annotation`, it retrieves the prohibited targets specified in the annotation.
     */
    def getProhibitedTargets(annotation: Annotation)(implicit p: SomeProject): List[String] = {
        val av = annotation.elementValuePairs collectFirst {
            case ElementValuePair("prohibitedTargets", ArrayValue(ab)) ⇒
                ab.map(elementValue => JVMType.toJavaType(elementValue.asInstanceOf[StringValue].value))
        }

        av.getOrElse(List()).toList
    }
}
