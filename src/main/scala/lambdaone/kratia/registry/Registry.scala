package lambdaone.kratia.registry

import cats.{Monad, ~>}
import lambdaone.toolbox.{CRUDStore, EventStore}

trait Registry[F[_], A, D] {

  def isMember(community: Community[A, D], member: Member[A, D]): F[Boolean]

  def load(community: Community[A, D], member: Member[A, D]): F[Option[D]]

  def loadAll(community: Community[A, D]): F[Iterable[D]]

  def register(community: Community[A, D], member: Member[A, D], data: D): F[Unit]
}

object Registry {

  implicit def registryCQRS[F[_], EventF[_], QueryF[_], A, D](
      implicit
      eventStore: EventStore[EventF, RegistryEvent],
      queryStore: CRUDStore[QueryF, (A, A), D],
      runEvent: EventF ~> F,
      runQuery: QueryF ~> F,
      F: Monad[F]): Registry[F, A, D] =
    new RegistryCQRS(eventStore, queryStore, runEvent, runQuery)
}

class RegistryCQRS[F[_], EventsF[_], QueryF[_], A, D] private[registry](
    eventStore: EventStore[EventsF, RegistryEvent],
    queryStore: CRUDStore[QueryF, (A, A), D],
    runEvent: EventsF ~> F,
    runQuery: QueryF ~> F
  ) extends Registry[F, A, D] {

  override def isMember(community: Community[A, D], member: Member[A, D]): F[Boolean] =
    runQuery(queryStore.exists(community.address -> member.address))

  override def load(community: Community[A, D], member: Member[A, D]): F[Option[D]] =
    runQuery(queryStore.get(community.address -> member.address))

  override def loadAll(community: Community[A, D]): F[Iterable[D]] =
    runQuery(queryStore.all)

  override def register(community: Community[A, D], member: Member[A, D], data: D): F[Unit] =
    runEvent(eventStore.emit(RegistryEvent.RegisterMember(community, member, data)))
}
