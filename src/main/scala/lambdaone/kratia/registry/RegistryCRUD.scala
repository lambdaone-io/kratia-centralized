package lambdaone.kratia.registry

import cats.Monad
import cats.implicits._
import lambdaone.toolbox.CRUDPick

case class RegistryCRUD[F[_]: Monad, D](query: CRUDPick[F, (Community, Member), D]) extends Registry[F, D] {

  override def isMember(community: Community, member: Member): F[Boolean] =
    query.exists(community -> member)

  override def load(community: Community, member: Member): F[Option[D]] =
    query.get(community -> member)

  override def loadAll(community: Community): F[List[D]] =
    query.filterId { case (c, _) => c == community }.map(_.values.toList)

  override def register(community: Community, member: Member, data: D): F[Unit] =
    query.create(data, community -> member).void

}

