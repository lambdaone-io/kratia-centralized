package lambdaone.kratia.protocol

import java.util.UUID

import cats.effect.{Clock, IO}
import cats.effect.concurrent.Ref
import lambdaone.kratia.collector.CollectorCRUD.BoxData
import lambdaone.kratia.collector.{BinaryProposal, Collector, CollectorCRUD}
import lambdaone.kratia.registry.{Registry, RegistryCRUD}
import lambdaone.toolbox.UniqueGen
import lambdaone.toolbox.mem.CRUDPickInMem

/**
  * Current unique community 19ce7b9b-a4da-4f9c-9838-c04fcb0ce9db
  */
case class Kratia[F[_], A](
  uniqueGen: UniqueGen[F, A],
  registry: Registry[F, A, MemberData],
  collector: Collector[F, A, BinaryProposal]
)

object Kratia {

  def inMem: IO[Kratia[IO, UUID]] =
    for {
      registry <- buildInMemRegistry
      collector <- buildInMemCollector
    } yield Kratia(UniqueGen.UniqueGenUUID, registry, collector)

  def buildInMemRegistry: IO[Registry[IO, UUID, MemberData]] =
    for {
      store <- Ref.of[IO, Map[(UUID, UUID), MemberData]](Map.empty)
      crud = CRUDPickInMem(store)
      reg = RegistryCRUD(crud)
    } yield reg

  def buildInMemCollector: IO[Collector[IO, UUID, BinaryProposal]] =
    for {
      store <- Ref.of[IO, Map[UUID, BoxData[UUID, BinaryProposal]]](Map.empty)
      clock = Clock.create[IO]
      crud = CRUDPickInMem(store)
      collector = CollectorCRUD[IO, UUID, BinaryProposal](clock, crud, UniqueGen.UniqueGenUUID)
    } yield collector
}
