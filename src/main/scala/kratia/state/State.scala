package kratia.state

import cats.implicits._
import cats.effect.Sync
import cats.effect.concurrent.Ref

trait State[F[_], A] {

  def get: F[A]

  def modify[B](f: A => (A, B)): F[B]

  def set(a: A): F[Unit]

  def update(f: A => A): F[Unit]
}

object State {

  def StateInMem[F[_], A](a: A)(implicit F: Sync[F]): F[State[F, A]] =
    Ref.of[F, A](a).map { ref =>
      new State[F, A] {

        override def get: F[A] =
          ref.get

        override def modify[B](f: A => (A, B)): F[B] =
          ref.modify(f)

        override def set(a: A): F[Unit] =
          ref.set(a)

        override def update(f: A => A): F[Unit] =
          ref.update(f)
      }
    }
}