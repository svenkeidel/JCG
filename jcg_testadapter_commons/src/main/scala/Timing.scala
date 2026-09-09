import play.api.libs.functional.syntax.{toFunctionalBuilderOps, unlift}
import play.api.libs.json.{JsNumber, JsObject, JsValue, Reads, Writes, __}

import scala.concurrent.duration.*

case class Time(nanoseconds: Duration, milliseconds: Duration, usedMemory: Long):
    def -(other: Time): Time = Time(this.nanoseconds - other.nanoseconds, this.milliseconds - other.milliseconds, this.usedMemory - other.usedMemory)

    override def toString: String = s"${nanoseconds.toUnit(SECONDS)} s (System.nanoTime), ${milliseconds.toUnit(SECONDS)} s (System.currentMillies), ${usedMemory / (1024 * 1024)} MB"

object Time:
    def apply(): Time = Time(nanoseconds = System.nanoTime().nanoseconds, milliseconds = System.currentTimeMillis().milliseconds, usedMemory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory())
    def fromNanoseconds(nanos: Long) = Time(nanoseconds = nanos.nanoseconds, milliseconds = nanos.nanoseconds.toMillis.milliseconds, usedMemory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory())
    def zero: Time = Time(0.nanoseconds, 0.milliseconds, usedMemory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory())
    def settleDown(): Unit = {
        System.gc()
        Thread.sleep(3.seconds.toMillis)
    }

given TimeWrites: Writes[Time] = time => JsObject(Map(
    "nanoseconds" -> JsNumber(time.nanoseconds.toNanos),
    "milliseconds" -> JsNumber(time.milliseconds.toMillis),
    "usedMemory" -> JsNumber(time.usedMemory)
))

given TimeReads: Reads[Time] = (
        (__ \ "nanoseconds").read[Long].map(_.nanoseconds) and
        (__ \ "milliseconds").read[Long].map(_.milliseconds) and
        (__ \ "usedMemory").read[Long]
    )(Time.apply(_,_,_))