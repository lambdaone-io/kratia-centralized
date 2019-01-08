package lambdaone.kratia.protocol

import java.util.UUID

import cats.effect.{Clock, ContextShift, IO, Timer}
import cats.effect.concurrent.Ref
import com.typesafe.config.{Config, ConfigFactory}
import doobie.util.{Get, Put}
import doobie.util.transactor.Transactor
import lambdaone.github.models.{Installation, InstallationAccessToken}
import lambdaone.github.{GitHubApi, GitHubApiIO, GitHubConfig}
import lambdaone.kratia.collector.{Collector, CollectorCRUD}
import lambdaone.kratia.collector.CollectorCRUD.BoxData
import lambdaone.kratia.registry.{Community, Member, Registry, RegistryCRUD}
import lambdaone.kratia.resolution.{Resolution, Resolved, ResolvedCRUD}
import lambdaone.toolbox.TemporalPriorityQueue.QueueItem
import lambdaone.toolbox.UniqueGen
import lambdaone.toolbox.mem.{CRUDPickInMem, TemporalPriorityQueueInMem}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object KratiaInMem {

  def buildConfigObject: IO[Config] =
    IO { ConfigFactory.load() }

  def inMem(implicit timer: Timer[IO], cs: ContextShift[IO]): IO[KratiaService] =
    for {
      config <- buildConfigObject
      registry <- buildInMemRegistry
      collector <- buildInMemCollector
      resolved <- buildInMemResolved
      queue <- buildResolutionQueue(timer)
      gitHubApi <- buildGitHubApi(config)
    } yield KratiaService(UniqueGen.UniqueGenUUID, registry, collector, resolved, queue, gitHubApi)

  def buildGitHubApi(config: Config)(implicit timer: Timer[IO], cs: ContextShift[IO]): IO[GitHubApi[IO]] =
    for {
      githubConfig <- GitHubConfig.load.run(config)
      crud <- CRUDPickInMem[Int, (Installation, InstallationAccessToken)]
      gitHubApi = GitHubApiIO(githubConfig, crud)
    } yield gitHubApi

  def buildInMemRegistry: IO[Registry[IO, MemberData]] =
    for {
      crud <- CRUDPickInMem[(Community, Member), MemberData]
      reg = RegistryCRUD(crud)
    } yield reg

  def buildInMemCollector(implicit timer: Timer[IO]): IO[Collector[IO]] =
    for {
      crud <- CRUDPickInMem[UUID, BoxData]
      collector = CollectorCRUD[IO](timer.clock, crud, UniqueGen.UniqueGenUUID)
    } yield collector

  def buildInMemResolved: IO[Resolved[IO]] =
    for {
      crud <- CRUDPickInMem[UUID, Resolution]
      resolved = ResolvedCRUD[IO](crud)
    } yield resolved

  def buildResolutionQueue(timer: Timer[IO]): IO[TemporalPriorityQueueInMem[UUID]] =
    for {
      ref <- Ref.of[IO, (List[(QueueItem[UUID], Long)], List[(QueueItem[UUID], Long)])](List.empty -> List.empty)
      queue = TemporalPriorityQueueInMem[UUID](
        t = timer,
        store = ref,
        initialTimeToWait = 1.second,
        tolerance = 3.second,
        incrementFactor = 1
      )
    } yield queue
}

object KratiaInDb {

  implicit val cs = IO.contextShift(ExecutionContext.global)

  implicit val xa = Transactor
    .fromDriverManager[IO](
      "org.h2.Driver",
      "jdbc:h2:mem:kratia;DB_CLOSE_DELAY=-1"
    )

  implicit val uuidGet: Get[UUID] = Get[String].map(UUID.fromString)
  implicit val uuidPut: Put[UUID] = Put[String].contramap(_.toString)

  /*
  def buildDbRegistry: IO[Registry[IO, MemberData]] =
    for {
      _ <- CrudPickSqlRegistry.init[IO]
      query = CrudPickSqlRegistry[IO, UUID, MemberData]
      reg = RegistryCRUD(query)
    } yield reg

  def inDb: IO[KratiaService] =
    for {
      registry <- buildDbRegistry
      collector <- buildDbCollector
    } yield KratiaService(UniqueGen.UniqueGenUUID, registry, collector)

  val clock: Clock[IO] = Clock.create[IO]

  val crudSqlCollector = CrudPickSqlCollector[IO, UUID, String]

  def buildDbCollector: IO[Collector[IO, UUID, BinaryProposal, String]] =
    for {
      _ <- CrudPickSqlCollector.init[IO]
      crudSqlCollector = CrudPickSqlCollector[IO, UUID, String]
      coll = CollectorCRUD[IO, UUID, BinaryProposal, String](
        clock,
        crudSqlCollector,
        UniqueGen.UniqueGenUUID
      )
    } yield coll
    */
}
