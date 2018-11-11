package lambdaone.toolbox

trait EventStore[F[_], A] { self =>

  def emit(event: A): F[Unit]

  def listen(callback: A => F[Unit]): F[Unit]

  def await(time: Long): F[Unit]

  def load: F[List[(Long, A)]]

  def last: F[(Long, A)]
}

object EventStore {

  implicit def apply[F[_], A](implicit store: EventStore[F, A]): EventStore[F, A] = store
}
