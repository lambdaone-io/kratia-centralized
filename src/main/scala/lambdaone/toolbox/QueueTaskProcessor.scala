package lambdaone.toolbox

import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.Timer
import cats.implicits._
import fs2.Stream
import fs2.concurrent.SignallingRef
import lambdaone.toolbox.QueueTaskProcessor.{WorkerShutDown, WorkerTask}
import lambdaone.toolbox.TemporalPriorityQueue.QueueItem

import scala.concurrent.duration._

class QueueTaskProcessor[A](
    parallelism: Int,
    restInterval: FiniteDuration,
    maxRetries: Int,
    queue: TemporalPriorityQueue[A]
  )(task: WorkerTask[A])(
    implicit
    cs: ContextShift[IO],
    timer: Timer[IO]
  ) {

  def body: IO[Unit] =
    queue.dequeue(parallelism).flatMap { data =>
      if (data.isEmpty)
        timer.sleep(restInterval)
      else
        data
          .traverse[IO, cats.effect.Fiber[IO, Unit]](process(_).start)
          .flatMap(_.foldLeft(IO.unit)(_ *> _.join))
    }

  def process(data: QueueItem[A]): IO[Unit] =
    task(data.data).attempt.flatMap {
      case Left(error) =>
        retryWithMessage(data, error.getMessage)
      case Right(Left(warning)) =>
        retryWithMessage(data, warning)
      case Right(Right(message)) =>
        queue.freeData(data) *> IO(println(s"[QueueTaskProcessor] finished $message"))
    }

  def retryWithMessage(data: QueueItem[A], message: String): IO[Unit] =
    if (data.iteration + 1 >= maxRetries)
      queue.freeData(data) *>
        IO(
          println(s"[QueueTaskProcessor] failed process with $message, (attempt: ${data.iteration}) will finally give up")
        )
    else
      queue.requeue(data).flatMap { duration =>
        IO(
          println(
            s"[QueueTaskProcessor] failed process with $message (attempt: ${data.iteration}) will try again in $duration"
          )
        )
      }

  def run: IO[WorkerShutDown] =
    for {
      finish <- SignallingRef[IO, Boolean](false)
      _ <- Stream.repeatEval(body).interruptWhen(finish).compile.drain.start
      shutdown = IO(println("[QueueTaskProcessor] shutting down queue task processor...")) *> finish.set(true)
    } yield shutdown
}

object QueueTaskProcessor {

  type WorkerShutDown = IO[Unit]

  type WorkerTask[A] = A => IO[Either[String, String]]
}

