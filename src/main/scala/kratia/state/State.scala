package kratia.state

import cats.effect.IO
import cats.effect.concurrent.Ref

trait State[F[_], A] {

  def get: F[A]

  def modify[B](f: A => (A, B)): F[B]

  def set(a: A): F[Unit]

  def update(f: A => A): F[Unit]
}

object State {

  def ref[A](a: A): IO[State[IO, A]] =
    Ref.of[IO, A](a).map { ref =>
      new State[IO, A] {

        override def get: IO[A] =
          ref.get

        override def modify[B](f: A => (A, B)): IO[B] =
          ref.modify(f)

        override def set(a: A): IO[Unit] =
          ref.set(a)

        override def update(f: A => A): IO[Unit] =
          ref.update(f)
      }
    }
}