package kratia

import cats.implicits._
import cats.effect.ConcurrentEffect
import fs2.Stream
import fs2.concurrent.SignallingRef

import scala.concurrent.ExecutionContext

package object utils {

  type Interrupt[F[_]] = F[Unit]

  def runWithInterrupt[F[_], A](stream: Stream[F, A])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): F[Interrupt[F]] =
    for {
      interrupt <- SignallingRef[F, Boolean](false)
      _ <- F.start(stream.interruptWhen[F](interrupt).compile.drain)
    } yield interrupt.set(false)

}
