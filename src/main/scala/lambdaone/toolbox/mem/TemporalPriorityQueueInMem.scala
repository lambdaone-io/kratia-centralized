package lambdaone.toolbox.mem

import cats.effect.concurrent.Ref
import cats.effect.{IO, Timer}
import cats.implicits._
import lambdaone.toolbox.TemporalPriorityQueue
import lambdaone.toolbox.TemporalPriorityQueue.QueueItem
import lambdaone.toolbox.mem.TemporalPriorityQueueInMem.WorkNoLongerInQueue

import scala.concurrent.duration.FiniteDuration

case class TemporalPriorityQueueInMem[A](
    t: Timer[IO],
    store: Ref[IO, (List[(QueueItem[A], Long)], List[(QueueItem[A], Long)])],
    initialTimeToWait: FiniteDuration,
    tolerance: FiniteDuration,
    incrementFactor: Int
  ) extends TemporalPriorityQueue[A] {

  override implicit def timer: Timer[IO] = t

  def log(message: String): IO[Unit] =
    IO(println(s"${Console.BLUE}Queue[-] $message${Console.RESET}"))

  protected def execEnqueue(data: A, readyOn: Long): IO[Unit] =
    log(s"Enqueue [readyOn = $readyOn](data = $data)") *>
      store.update {
        case (dataStore, taken) =>
          ((QueueItem(data), readyOn) :: dataStore, taken)
      }

  protected def execDequeue(now: Long, staleOn: Long, amount: Int): IO[List[QueueItem[A]]] =
    for {
      taken <- store.modify {
        case (dataStore, taken) =>
          val partition = dataStore.partition(_._2 <= now)
          val (matured, young) = (partition._1.take(amount), partition._2 ++ partition._1.drop(amount))
          val toTake = matured.map(t => (t._1, staleOn))
          ((young, taken ++ toTake), toTake.map(_._1))
      }
      _ <- log(s"Took: ${taken.mkString(", ")}")
    } yield taken

  protected def execStaleCheck(now: Long): IO[Unit] =
    for {
      staled <- store.modify {
        case (dataStore, taken) =>
          val (matured, young) = taken.partition(_._2 <= now)
          val staled = matured.map(t => (t._1.refreshKey, now))
          ((dataStore ++ staled, young), staled.map(_._1))
      }
      _ <- log(s"Stale data: ${staled.mkString(", ")}")
    } yield ()

  protected def execRequeue(next: QueueItem[A], readyOn: Long): IO[Unit] =
    for {
      requeue <- store.modify {
        case (dataStore, taken) =>
          val (isData, stillTaken) = taken.partition(_._1.key == next.key)
          val requeue = isData.map(_ => (next, readyOn))
          ((dataStore ++ requeue, stillTaken), requeue.map(_._1))
      }
      _ <- {
        if (requeue.isEmpty) IO.raiseError(WorkNoLongerInQueue(next.extract))
        else log(s"Requeue data: ${requeue.mkString(", ")}")
      }
    } yield ()

  def freeData(data: QueueItem[A]): IO[Unit] =
    for {
      free <- store.modify {
        case (dataStore, taken) =>
          val (isData, stillTaken) = taken.partition(_._1.key == data.key)
          ((dataStore, stillTaken), isData.map(_._1))
      }
      _ <- {
        if (free.isEmpty) IO.raiseError(WorkNoLongerInQueue(data.extract))
        else log(s"Free data: ${free.mkString(", ")}")
      }
    } yield ()

  override protected def execDelete(data: A): IO[Unit] =
    for {
      free <- store.modify {
        case (dataStore, taken) =>
          val (isData, keep) = dataStore.partition(_._1 == data)
          ((keep, taken), isData.map(_._1))
      }
      _ <- {
        if (free.isEmpty) IO.raiseError(WorkNoLongerInQueue(data))
        else log(s"Deleted data: ${free.mkString(", ")}")
      }
    } yield ()

  def size: IO[Int] =
    store.get.flatMap {
      case (data, _) =>
        log(s"Size is ${data.length}") *> IO.pure(data.length)
    }

  def taken: IO[Int] =
    store.get.flatMap {
      case (_, taken) =>
        log(s"Taken are ${taken.length}") *> IO.pure(taken.length)
    }

}

object TemporalPriorityQueueInMem {

  case class WorkNoLongerInQueue[W](work: W) extends RuntimeException
}
