package kratia

import cats.implicits._
import cats.effect.ConcurrentEffect
import fs2.Stream
import fs2.async.mutable.Signal

import scala.concurrent.ExecutionContext

package object utils {

  type Interrupt[F[_]] = F[Unit]

  def runWithInterrupt[F[_], A](stream: Stream[F, A])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): F[Interrupt[F]] =
    for {
      interrupt <- Signal[F, Boolean](false)
      _ <- F.start(stream.interruptWhen(interrupt).compile.drain)
    } yield interrupt.set(false)

}
