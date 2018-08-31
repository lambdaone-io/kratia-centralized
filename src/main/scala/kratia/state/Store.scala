package kratia.state

import java.util.UUID

import cats.implicits._
import cats.{Functor, Monad, MonadError}

trait Store[F[_], A] {

  def create(a: A): F[Ins[A]]

  def read(id: UUID): F[Option[Ins[A]]]

  def get(id: UUID)(e: => Throwable)(implicit F: MonadError[F, Throwable]): F[Ins[A]] =
    read(id) >>= {
      case None => F.raiseError(e)
      case Some(a) => F.pure(a)
    }

  def update(id: UUID)(f: A => A): F[Option[Ins[A]]]

  def delete(id: UUID): F[Option[Ins[A]]]

  def exists(a: A): F[Boolean]

  def set(id: UUID)(newValue: A): F[Option[Ins[A]]] =
    update(id)(_ => newValue)

  def delete_(id: UUID)(implicit F: Functor[F]): F[Unit] =
    delete(id).void
}

object Store {

  def fromState[F[_], A](state: State[F, Map[UUID, A]])(implicit F: Monad[F]): Store[F, A] =
    new Store[F, A] {

      override def create(a: A): F[Ins[A]] = {
        val id = UUID.randomUUID()
        state.update(_ + (id -> a)) *> F.pure(Ins(id, a))
      }

      override def read(id: UUID): F[Option[Ins[A]]] =
        state.get.map(_.get(id).map(Ins(id, _)))

      override def update(id: UUID)(f: A => A): F[Option[Ins[A]]] =
        read(id) >>= {
          case Some(a) =>
            val updated = f(a.model)
            state.update(_ + (id -> updated)) *> F.pure(Some(Ins(id, updated)))
          case None =>
            F.pure(None)
        }

      override def delete(id: UUID): F[Option[Ins[A]]] =
        read(id) >>= {
          case Some(a) =>
            state.update(_ - id) *> F.pure(Some(a))
          case None =>
            F.pure(None)
        }

      override def exists(a: A): F[Boolean] =
        state.get.map(_.exists(_._2 == a))
    }
}