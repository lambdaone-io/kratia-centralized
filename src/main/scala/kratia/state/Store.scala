package kratia.state

import java.util.UUID

import cats.implicits._
import cats.Functor

trait Store[F[_], A] {

  def create(a: A): F[Ins[A]]

  def read(id: UUID): F[Option[Ins[A]]]

  def update(id: UUID)(f: A => A): F[Option[Ins[A]]]

  def delete(id: UUID): F[Option[Ins[A]]]

  def exists(a: A): F[Boolean]

  def set(id: UUID)(newValue: A): F[Option[Ins[A]]] =
    update(id)(_ => newValue)

  def delete_(id: UUID)(implicit F: Functor[F]): F[Unit] =
    delete(id).void
}
