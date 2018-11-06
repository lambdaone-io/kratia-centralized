package lambdaone.kratia.registry

import cats.{Monad, ~>}
import lambdaone.toolbox.{CRUDStore, EventStore, Interpreter}

trait Registry[F[_], A, D] {

  def isMember(community: Community[A, D], member: Member[A, D]): F[Boolean]

  def load(community: Community[A, D], member: Member[A, D]): F[Option[D]]

  def loadAll(community: Community[A, D]): F[Iterable[D]]

  def register(community: Community[A, D], member: Member[A, D], data: D): F[Unit]
}

object Registry {

  implicit def registryCQRS[F[_]: Monad, EventF[_], QueryF[_], A, D](
      implicit
      event: Interpreter[EventStore[?, RegistryEvent], EventF, F],
      query: Interpreter[CRUDStore[?, (A, A), D], QueryF, F]
    ): Registry[F, A, D] =
      new RegistryCQRS(event, query)
}

class RegistryCQRS[F[_], EventF[_], QueryF[_], A, D] private[registry](
    event: Interpreter[EventStore[?, RegistryEvent], EventF, F],
    query: Interpreter[CRUDStore[?, (A, A), D], QueryF, F]
  ) extends Registry[F, A, D] {

  override def isMember(community: Community[A, D], member: Member[A, D]): F[Boolean] =
    query.run(_.exists(community.address -> member.address))

  override def load(community: Community[A, D], member: Member[A, D]): F[Option[D]] =
    query.run(_.get(community.address -> member.address))

  override def loadAll(community: Community[A, D]): F[Iterable[D]] =
    query.run(_.all)

  override def register(community: Community[A, D], member: Member[A, D], data: D): F[Unit] =
    event.run(_.emit(RegistryEvent.RegisterMember(community, member, data)))
}
