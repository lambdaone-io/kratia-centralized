package lambdaone.toolbox

import cats.effect.Sync
import cats.implicits._
import cats.effect.concurrent.Ref

trait State[F[_], A] {

  def get: F[A]

  def modify[B](f: A => (A, B)): F[B]

  def set(a: A): F[A] =
    modify(_ => (a, a))

  def update(f: A => A): F[A] =
    modify(a => (f(a), f(a)))
}

object State {

  def inMem[F[_], A](a: A)(implicit F: Sync[F]): F[State[F, A]] =
    Ref.of[F, A](a).map { ref =>
      new State[F, A] {

        override def get: F[A] =
          ref.get

        override def modify[B](f: A => (A, B)): F[B] =
          ref.modify(f)
      }
    }
}