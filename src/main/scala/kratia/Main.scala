package kratia

import cats.effect.Timer
import fs2.Stream
import kratia.Configuration.KratiaConfig
import scala.concurrent.duration._

object Main {


  case class Kratia()
  case class Time(value: Long) extends AnyVal


  def timeStream[F[_]](config: KratiaConfig)(implicit timer: Timer[F]): Stream[F, Time] =
    Stream
      .fixedRate(config.appSpeed)
      .evalMap(_ => timer.clock.monotonic(MILLISECONDS))
      .map(Time)
}
