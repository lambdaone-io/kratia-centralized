package kratia.helpers

import cats.effect.{ConcurrentEffect, IO, Timer}
import fs2.concurrent.Queue
import kratia.helpers.KratiaSuite.{Context, TestFailure}
import kratia.App
import kratia.Protocol.{InMessage, OutMessage}
import kratia.utils._
import lambdaone.toolbox.State
import org.scalatest.{Assertion, AsyncFunSuite}

import scala.concurrent.Future
import scala.concurrent.duration._

abstract class KratiaSuite extends AsyncFunSuite {

  implicit val timer: Timer[IO] = IO.timer(executionContext)

  /*
  def exec(f: Context => IO[Assertion]): Future[Assertion] =
    (for {
      kratia <- App.KratiaInMem[IO].compile.toList
      connection <- App.connect[IO](kratia.head, ConcurrentEffect[IO], executionContext)
      testSendQueue <- fs2.async.unboundedQueue[IO, InMessage]
      receiveCache <- State.inMem[IO, List[OutMessage]](List.empty)
      interruptSend <- runWithInterrupt(testSendQueue.dequeue.to(connection.receive))
      interruptReceive <- runWithInterrupt(connection.send.to { messages =>
        messages.evalMap(message => receiveCache.update(_ :+ message)).map(_ => ())
      })
      context = Context(kratia.head, connection, testSendQueue, receiveCache)
      assertionAttempt <- f(context).attempt
      _ <- interruptSend
      _ <- interruptReceive
    } yield assertionAttempt).unsafeToFuture().flatMap {
      case Left(TestFailure(reason)) => Future.successful(fail(reason))
      case Left(e) => Future.failed(e)
      case Right(a) => Future.successful(a)
    }
    */
}

object KratiaSuite {

  case class TestFailure(reason: String) extends Throwable

  def raise[A](reason: String): IO[A] = IO.raiseError(TestFailure(reason))

  case class Context(
                      kratia: Kratia[IO],
                      connection: Connection[IO],
                      testSendQueue: Queue[IO, InMessage],
                      receiveCache: State[IO, List[OutMessage]]
                    )(implicit timer: Timer[IO]) {

    def ! (message: InMessage): IO[Unit] =
      testSendQueue.enqueue1(message)

    def within(duration: FiniteDuration): Within = Within(duration, this)
  }

  case class Within(duration: FiniteDuration, context: Context)(implicit timer: Timer[IO]) {

    def expect(description: String)(matcher: OutMessage => Boolean): IO[List[OutMessage]] = {
      def iteration(i: FiniteDuration): IO[List[OutMessage]] = {
        if (i >= duration) raise(s"Timedout ($duration) while expected $description")
        else IO.sleep(i).flatMap { _ =>
          for {
            matches <- context.receiveCache.modify(_.partition(matcher).swap)
            message <- {
              if (matches.isEmpty)
                iteration(i + 50.millis)
              else
                IO(matches)
            }
          } yield message
        }
      }
      iteration(50.millis)
    }

    def expectOne(description: String)(matcher: OutMessage => Boolean): IO[OutMessage] = {
      def iteration(i: FiniteDuration): IO[OutMessage] = {
        if (i >= duration) raise(s"Timedout ($duration) while expected $description")
        else IO.sleep(i).flatMap { _ =>
          for {
            matches <- context.receiveCache.modify(_.partition(matcher).swap)
            message <- {
              if (matches.isEmpty)
                iteration(i + 50.millis)
              else if (matches.length > 1)
                raise(s"Expected 1 $description after $duration, but got several $matches")
              else
                IO(matches.head)
            }
          } yield message
        }
      }
      iteration(50.millis)
    }
  }
}

