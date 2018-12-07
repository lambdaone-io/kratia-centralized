package lambdaone.kratia.protocol

import java.util.UUID

import cats.effect.{Clock, IO}
import cats.effect.concurrent.Ref
import doobie.util.{Get, Put}
import doobie.util.transactor.Transactor
import lambdaone.kratia.collector.{BinaryProposal, Collector, CollectorCRUD}
import lambdaone.kratia.collector.CollectorCRUD.BoxData
import lambdaone.kratia.registry.{Registry, RegistryCRUD}
import lambdaone.toolbox.UniqueGen
import lambdaone.toolbox.mem.CRUDPickInMem
import lambdaone.toolbox.sql.{CrudPickSqlCollector, CrudPickSqlRegistry}

import scala.concurrent.ExecutionContext


object KratiaInMem {

  def inMem: IO[KratiaService] =
    for {
      registry <- buildInMemRegistry
      collector <- buildInMemCollector
    } yield KratiaService(UniqueGen.UniqueGenUUID, registry, collector)

  def buildInMemRegistry: IO[Registry[IO, UUID, MemberData]] =
    for {
      store <- Ref.of[IO, Map[(UUID, UUID), MemberData]](Map.empty)
      crud = CRUDPickInMem(store)
      reg = RegistryCRUD(crud)
    } yield reg

  def buildInMemCollector: IO[Collector[IO, UUID, BinaryProposal, String]] =
    for {
      store <- Ref.of[IO, Map[UUID, BoxData[UUID, BinaryProposal, String]]](Map.empty)
      clock = Clock.create[IO]
      crud = CRUDPickInMem(store)
      collector = CollectorCRUD[IO, UUID, BinaryProposal, String](clock, crud, UniqueGen.UniqueGenUUID)
    } yield collector
}

object KratiaInDb {

  implicit val cs = IO.contextShift(ExecutionContext.global)

  implicit val xa = Transactor
    .fromDriverManager[IO]("org.h2.Driver", "jdbc:h2:mem:kratia;DB_CLOSE_DELAY=-1")

  implicit val uuidGet: Get[UUID] = Get[String].map(UUID.fromString(_))
  implicit val uuidPut: Put[UUID] = Put[String].contramap(_.toString)

  def buildDbRegistry: IO[Registry[IO, UUID, MemberData]] =
    for {
      _ <- CrudPickSqlRegistry.createTable[IO]
      query = CrudPickSqlRegistry[IO, UUID, MemberData]
      reg = RegistryCRUD(query)
    } yield reg


  def inDb: IO[KratiaService] =
    for {
      registry <- buildDbRegistry
      collector <- buildInDbCollector
    } yield KratiaService(UniqueGen.UniqueGenUUID, registry, collector)

  val clock = Clock.create[IO]

  implicit val readBox: Get[BoxData[UUID, BinaryProposal, String]] = ???

  val crudSqlCollector = CrudPickSqlCollector[IO, UUID,  String]

  def buildInDbCollector: IO[Collector[IO, UUID, BinaryProposal, String]] =
    IO.pure(CollectorCRUD[IO, UUID, BinaryProposal, String](clock, crudSqlCollector, UniqueGen.UniqueGenUUID))
}
