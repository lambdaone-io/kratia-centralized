package lambdaone.toolbox

import java.util.UUID

import cats.effect.IO
import cats.effect.Timer
import lambdaone.toolbox.TemporalPriorityQueue.QueueItem

import scala.concurrent.duration._

trait TemporalPriorityQueue[A] {

  implicit def timer: Timer[IO]

  def initialTimeToWait: FiniteDuration

  def tolerance: FiniteDuration

  def incrementFactor: Int

  protected def execEnqueue(data: A, readyOn: Long): IO[Unit]

  protected def execDequeue(now: Long, staleOn: Long, amount: Int): IO[List[QueueItem[A]]]

  protected def execStaleCheck(now: Long): IO[Unit]

  protected def execRequeue(next: QueueItem[A], readyOn: Long): IO[Unit]

  protected def execDelete(data: A): IO[Unit]

  def freeData(data: QueueItem[A]): IO[Unit]

  def size: IO[Int]

  def taken: IO[Int]

  def isEmpty: IO[Boolean] = size.map(_ == 0)

  def enqueue(data: A, ttw: FiniteDuration): IO[FiniteDuration] =
    for {
      now <- timer.clock.realTime(SECONDS)
      _ <- execEnqueue(data, now + ttw.toSeconds)
    } yield ttw

  def dequeue(amount: Int): IO[List[QueueItem[A]]] =
    for {
      now <- timer.clock.realTime(SECONDS)
      _ <- execStaleCheck(now)
      data <- execDequeue(now, staleTime(now), amount)
    } yield data

  def requeue(data: QueueItem[A]): IO[FiniteDuration] =
    for {
      now <- timer.clock.realTime(SECONDS)
      next = data.successor
      ttw = timeToWait(next.iteration)
      _ <- execRequeue(next, now + ttw.toSeconds)
    } yield ttw

  def delete(data: A): IO[Unit] =
    execDelete(data)

  private def timeToWait(iteration: Int): FiniteDuration =
    initialTimeToWait * Math.pow(incrementFactor.toDouble, (iteration - 1).toDouble).toLong

  private def staleTime(now: Long): Long =
    now + tolerance.toSeconds
}

object TemporalPriorityQueue {

  case class QueueItem[A](key: UUID, data: A, iteration: Int) {

    def extract: A = data

    def map[B](f: A => B): QueueItem[B] = copy(key, f(data), iteration)

    def refreshKey: QueueItem[A] = copy(key = UUID.randomUUID())

    def successor: QueueItem[A] = copy(key, data, iteration + 1)
  }

  object QueueItem {

    def apply[A](data: A): QueueItem[A] = new QueueItem[A](UUID.randomUUID(), data, 1)
  }
}

