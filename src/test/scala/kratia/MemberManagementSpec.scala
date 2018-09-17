package kratia

import cats.effect._
import kratia.kratia_app.{Connection, Kratia}
import org.scalatest.{Assertion, AsyncFunSuite}
import KratiaSuite._
import fs2.Stream
import fs2.async.mutable.{Queue, Signal}
import kratia.kratia_protocol.{InMessage, OutMessage}

import scala.concurrent.{ExecutionContext, Future}

class MemberManagementSpec extends KratiaSuite {

  test("1")
}

abstract class KratiaSuite extends AsyncFunSuite {

  implicit val timer: Timer[IO] = IO.timer(executionContext)

  def exec(f: Context => IO[Assertion]): Future[Assertion] =
    (for {
      kratia <- kratia_app.KratiaInMem[IO].compile.toList
      connection <- kratia_app.connect[IO](kratia.head, ConcurrentEffect[IO], executionContext)
      testSendQueue <- fs2.async.unboundedQueue[IO, InMessage]
      testReceiveQueue <- fs2.async.unboundedQueue[IO, OutMessage]
      interruptSend <- runWithInterrupt(testSendQueue.dequeue.to(connection.receive))
      interruptReceive <- runWithInterrupt(connection.send.to(testReceiveQueue.enqueue))
      context = Context(kratia.head, connection, testSendQueue, testReceiveQueue)
      assertion <- f(context)
      _ <- interruptSend
      _ <- interruptReceive
    } yield assertion).unsafeToFuture()

  type Interrupt[F[_]] = F[Unit]

  private def runWithInterrupt[F[_], A](stream: Stream[F, A])(implicit F: Effect[F], ec: ExecutionContext): F[Interrupt[F]] =
    for {
      interrupt <- Signal[F, Boolean](false)
      _ <- stream.interruptWhen(interrupt).compile.drain
    } yield interrupt.set(false)
}

object KratiaSuite {

  case class Context(kratia: Kratia[IO], connection: Connection[IO], testSendQueue: Queue[IO, InMessage], testReceiveQueue: Queue[IO, OutMessage]) {

    def ! (message: InMessage): IO[Unit] =
      connection.
  }
}
