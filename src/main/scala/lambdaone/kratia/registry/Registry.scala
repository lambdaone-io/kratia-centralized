package lambdaone.kratia.registry

import cats.{Functor, Monad}
import cats.implicits._
import lambdaone.toolbox.CRUDPick

object Registry {

  implicit def apply[F[_]: Monad, D](implicit query: CRUDPick[F, (Community, Member), D]): Registry[F, D] =
    new RegistryCRUD[F, D](query)
}

trait Registry[F[_], D] { self =>

  def isMember(community: Community, member: Member): F[Boolean]

  def load(community: Community, member: Member): F[Option[D]]

  def loadAll(community: Community): F[List[D]]

  def register(community: Community, member: Member, data: D): F[Unit]

  final def imap[D0](f: D => D0)(g: D0 => D)(implicit F: Functor[F]): Registry[F, D0] =
    new Registry[F, D0] {
      override def isMember(community: Community, member: Member): F[Boolean] =
        self.isMember(community, member)

      override def load(community: Community, member: Member): F[Option[D0]] =
        self.load(community, member).map(_.map(f))

      override def loadAll(community: Community): F[List[D0]] =
        self.loadAll(community).map(_.map(f))

      override def register(community: Community, member: Member, data: D0): F[Unit] =
        self.register(community, member, g(data))
    }
}

