package lambdaone.toolbox

import fs2.Stream
import fs2.concurrent.Queue

trait EventStore[F[_], A] {

  def emit(event: A): F[Unit]

  def load: F[List[A]]

  def listen: Stream[F, A]
}

object EventStore {

  implicit def eventStoreReference[F[_], A](implicit store: StateMonad[F, (List[A], Queue[F, A])]): EventStore[F, A] =
    new EventStoreReference(store)
}

class EventStoreReference[F[_], A] private[toolbox] (store: StateMonad[F, (List[A], Queue[F, A])]) extends EventStore[F, A] {

  override def emit(event: A): F[Unit] =
    store.modify { case (xs, queue) => (event :: xs) -> queue }

  override def load: F[List[A]] =
    store.map(store.get)(_._1)

  override def listen: Stream[F, A] =
    for {
      queue <- Stream.eval[F, Queue[F, A]](store.map(store.get)(_._2))
      event <- queue.dequeue
    } yield event
}