package lambdaone.kratia.registry

import cats.Monad
import lambdaone.toolbox.{EventStore, Store}
import cats.implicits._

trait Registry[F[_], A] {

  def isMember(community: Community[A], member: Member[A]): F[Boolean]

  def load(community: Community[A], member: Member[A]): F[Option[A]]

  def loadAll(community: Community[A]): F[List[A]]

  def register(community: Community[A], member: Member[A], data: A): F[Unit]
}

object Registry {

  implicit def registryWithEventStore[F[_], A](implicit store: EventStore[F, RegistryEvent[A]], F: Monad[F]): Registry[F, A] =
    new Registry[F, A] {

      override def isMember(community: Community[A], member: Member[A]): F[Boolean] =
        store.find {
          case RegistryEvent.RegisterMember(community0, member0, _) => community == community0 && member == member0
        }.map(_.isDefined)

      override def load(community: Community[A], member: Member[A]): F[Option[A]] =
        store.findMap {
          case RegistryEvent.RegisterMember(community0, member0, data) if community == community0 && member == member0 => data
        }

      override def loadAll(community: Community[A]): F[List[A]] =
        store.filterMap {
          case RegistryEvent.RegisterMember(community0, _, data) if community == community0 => data
        }

      override def register(community: Community[A], member: Member[A], data: A): F[Unit] =
        store.append(RegistryEvent.RegisterMember(community, member, data)).void
    }
}
