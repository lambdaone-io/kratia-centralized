package lambdaone.kratia.registry

import cats.Monad
import lambdaone.toolbox.CRUDPick

object Registry {

  implicit def apply[F[_]: Monad, A, D](implicit query: CRUDPick[F, (A, A), D]): Registry[F, A, D] =
    new RegistryCRUD[F, A, D](query)
}

trait Registry[F[_], A, D] {

  def isMember(community: Community[A, D], member: Member[A, D]): F[Boolean]

  def load(community: Community[A, D], member: Member[A, D]): F[Option[D]]

  def loadAll(community: Community[A, D]): F[List[D]]

  def register(community: Community[A, D], member: Member[A, D], data: D): F[Unit]
}

