import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsNumber, JsObject, JsValue, Reads, Writes, __}

import scala.concurrent.duration.*

case class Time(nanoseconds: Duration, milliseconds: Duration):
    def -(other: Time): Time = Time(this.nanoseconds - other.nanoseconds, this.milliseconds - other.milliseconds)

    override def toString: String = s"${nanoseconds.toUnit(SECONDS)}s (System.nanoTime), ${milliseconds.toUnit(SECONDS)}s (System.currentMillies)"

object Time:
    def apply(): Time = Time(nanoseconds = System.nanoTime().nanoseconds, milliseconds = System.currentTimeMillis().milliseconds)
    def fromNanoseconds(nanos: Long) = Time(nanoseconds = nanos.nanoseconds, milliseconds = nanos.nanoseconds.toMillis.milliseconds)
    def zero: Time = Time(0.nanoseconds, 0.milliseconds)

given TimeWrites: Writes[Time] = time => JsObject(Map(
    "nanoseconds" -> JsNumber(time.nanoseconds.toNanos),
    "milliseconds" -> JsNumber(time.milliseconds.toMillis)
))

given TimeReads: Reads[Time] = (
        (__ \ "nanoseconds").read[Long].map(_.nanoseconds) and
        (__ \ "milliseconds").read[Long].map(_.milliseconds)
    )(Time.apply(_,_))