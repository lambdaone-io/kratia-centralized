package kratia.state

import java.util.UUID

import cats.implicits._
import cats.{Functor, Monad, MonadError}

trait Store[F[_], A] {

  def create(a: A): F[Instance[A]]

  def read(id: UUID): F[Option[Instance[A]]]

  def get(id: UUID)(e: => Throwable)(implicit F: MonadError[F, Throwable]): F[Instance[A]] =
    read(id) >>= {
      case None => F.raiseError(e)
      case Some(a) => F.pure(a)
    }

  def update(id: UUID)(f: A => A): F[Option[Instance[A]]]

  def delete(id: UUID): F[Option[Instance[A]]]

  def all: F[List[A]]

  def exists(a: A): F[Boolean]

  def filter(f: A => Boolean)(implicit F: Functor[F]): F[List[A]] =
    all.map(_.filter(f))

  def set(id: UUID)(newValue: A): F[Option[Instance[A]]] =
    update(id)(_ => newValue)

  def delete_(id: UUID)(implicit F: Functor[F]): F[Unit] =
    delete(id).void
}

object Store {

  def StoreFromState[F[_], A](state: State[F, Map[UUID, A]])(implicit F: Monad[F]): Store[F, A] =
    new Store[F, A] {

      override def create(a: A): F[Instance[A]] = {
        val id = UUID.randomUUID()
        state.update(_ + (id -> a)) *> F.pure(Instance(id, a))
      }

      override def read(id: UUID): F[Option[Instance[A]]] =
        state.get.map(_.get(id).map(Instance(id, _)))

      override def update(id: UUID)(f: A => A): F[Option[Instance[A]]] =
        read(id) >>= {
          case Some(a) =>
            val updated = f(a.model)
            state.update(_ + (id -> updated)) *> F.pure(Some(Instance(id, updated)))
          case None =>
            F.pure(None)
        }

      override def delete(id: UUID): F[Option[Instance[A]]] =
        read(id) >>= {
          case Some(a) =>
            state.update(_ - id) *> F.pure(Some(a))
          case None =>
            F.pure(None)
        }

      override def all: F[List[A]] =
        state.get.map(_.values.toList)

      override def exists(a: A): F[Boolean] =
        state.get.map(_.exists(_._2 == a))
    }
}