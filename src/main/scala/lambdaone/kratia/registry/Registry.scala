package lambdaone.kratia.registry

import cats.Monad
import lambdaone.toolbox.{CRUDStore, EventStore}

trait Registry[F[_], A, D] {

  def isMember(community: Community[A, D], member: Member[A, D]): F[Boolean]

  def load(community: Community[A, D], member: Member[A, D]): F[Option[D]]

  def loadAll(community: Community[A, D]): F[List[D]]

  def register(community: Community[A, D], member: Member[A, D], data: D): F[Unit]
}

object Registry {

  implicit def registryCQRS[F[_]: Monad, A, D](
      implicit
      event: EventStore[F, RegistryEvent],
      query: CRUDStore[F, (A, A), D],
    ): Registry[F, A, D] =
      new RegistryCQRS(event, query)
}

class RegistryCQRS[F[_], A, D] private[registry](
    event: EventStore[F, RegistryEvent],
    query: CRUDStore[F, (A, A), D],
  ) extends Registry[F, A, D] {

  override def isMember(community: Community[A, D], member: Member[A, D]): F[Boolean] =
    query.exists(community.address -> member.address)

  override def load(community: Community[A, D], member: Member[A, D]): F[Option[D]] =
    query.get(community.address -> member.address)

  override def loadAll(community: Community[A, D]): F[List[D]] =
    query.all

  override def register(community: Community[A, D], member: Member[A, D], data: D): F[Unit] =
    event.emit(RegistryEvent.RegisterMember(community, member, data))
}
