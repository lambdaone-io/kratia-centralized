package lambdaone.kratia.resolution

import java.util.UUID

import cats.Monad
import cats.implicits._
import lambdaone.toolbox.CRUDPick

class ResolvedCRUD[F[_]: Monad](store: CRUDPick[F, UUID, Resolution]) extends Resolved[F] {

  override def create(resolution: Resolution): F[Unit] = {
    for {
      _ <- store.create(resolution, resolution.address)
    } yield ()
  }

  override def listClosed: F[List[Resolution]] =
    store.all.map(_.values.toList)

}
