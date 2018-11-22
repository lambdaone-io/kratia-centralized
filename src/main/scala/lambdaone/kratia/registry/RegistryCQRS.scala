package lambdaone.kratia.registry

import cats.Monad
import cats.implicits._
import lambdaone.toolbox.{CRUDStore, EventStore}

case class RegistryCQRS[F[_]: Monad, A, D](
                                            event: EventStore[F, RegistryEvent],
                                            query: CRUDStore[F, (A, A), D],
                                          ) extends Registry[F, A, D] {

  override def isMember(community: Community[A, D], member: Member[A, D]): F[Boolean] =
    query.exists(community.address -> member.address)

  override def load(community: Community[A, D], member: Member[A, D]): F[Option[D]] =
    query.get(community.address -> member.address)

  override def loadAll(community: Community[A, D]): F[List[D]] =
    query.filterId { case (c, _) => c == community.address }.map(_.values.toList)

  override def register(community: Community[A, D], member: Member[A, D], data: D): F[Unit] =
    event.emit(RegistryEvent.RegisterMember(community, member, data))
}


