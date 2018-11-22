package lambdaone.kratia.protocol

import java.util.UUID

import cats.effect.IO
import cats.effect.concurrent.Ref
import lambdaone.kratia.registry.{Registry, RegistryCRUD}
import lambdaone.toolbox.UniqueGen
import lambdaone.toolbox.mem.CRUDPickInMem

/**
  * Current unique community 19ce7b9b-a4da-4f9c-9838-c04fcb0ce9db
  */
case class Kratia[F[_], A](
  uniqueGen: UniqueGen[F, A],
  registry: Registry[F, A, MemberData]
)

object Kratia {

  def inMem: IO[Kratia[IO, UUID]] =
    for {
      registry <- buildInMemRegistry
    } yield Kratia(UniqueGen.UniqueGenUUID, registry)

  def buildInMemRegistry: IO[Registry[IO, UUID, MemberData]] =
    for {
      store <- Ref.of[IO, Map[(UUID, UUID), MemberData]](Map.empty)
      crud = CRUDPickInMem(store)
      reg = RegistryCRUD(crud)
    } yield reg

}
