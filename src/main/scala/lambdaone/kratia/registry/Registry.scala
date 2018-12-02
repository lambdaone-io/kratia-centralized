package lambdaone.kratia.registry

import cats.{Functor, Monad}
import cats.implicits._
import lambdaone.toolbox.CRUDPick

object Registry {

  implicit def apply[F[_]: Monad, A, D](query: CRUDPick[F, (A, A), D]): Registry[F, A, D] =
    new RegistryCRUD[F, A, D](query)
}

trait Registry[F[_], A, D] { self =>

  def isMember(community: Community[A, D], member: Member[A, D]): F[Boolean]

  def load(community: Community[A, D], member: Member[A, D]): F[Option[D]]

  def loadAll(community: Community[A, D]): F[List[D]]

  def register(community: Community[A, D], member: Member[A, D], data: D): F[Unit]

  final def imap[D0](f: D => D0)(g: D0 => D)(implicit F: Functor[F]): Registry[F, A, D0] =
    new Registry[F, A, D0] {
      override def isMember(community: Community[A, D0], member: Member[A, D0]): F[Boolean] =
        self.isMember(community.map(g), member.map(g))

      override def load(community: Community[A, D0], member: Member[A, D0]): F[Option[D0]] =
        self.load(community.map(g), member.map(g)).map(_.map(f))

      override def loadAll(community: Community[A, D0]): F[List[D0]] =
        self.loadAll(community.map(g)).map(_.map(f))

      override def register(community: Community[A, D0], member: Member[A, D0], data: D0): F[Unit] =
        self.register(community.map(g), member.map(g), g(data))
    }
}

