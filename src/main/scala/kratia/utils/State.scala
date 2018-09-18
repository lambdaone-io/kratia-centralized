package kratia.utils

import cats.effect.Sync
import cats.implicits._
import fs2.async.Ref

trait State[F[_], A] {

  def get: F[A]

  def modify[B](f: A => (A, B)): F[B]

  def set(a: A): F[A]

  def update(f: A => A): F[A]
}

object State {

  def inMem[F[_], A](a: A)(implicit F: Sync[F]): F[State[F, A]] =
    Ref[F, A](a).map { ref =>
      new State[F, A] {

        override def get: F[A] =
          ref.get

        override def modify[B](f: A => (A, B)): F[B] =
          ref.modify2(f).map(_._2)

        override def set(a: A): F[A] =
          ref.modify(_ => a).map(_.now)

        override def update(f: A => A): F[A] =
          ref.modify(f).map(_.now)
      }
    }
}