package kratia.state

trait State[F[_], A] {

  def get: F[A]

  def modify[B](f: A => (A, B)): F[B]

  def set(a: A): F[A]

  def update(f: A => A): F[A]
}
