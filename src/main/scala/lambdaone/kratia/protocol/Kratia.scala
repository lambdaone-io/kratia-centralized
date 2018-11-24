package lambdaone.kratia.protocol

import java.util.UUID

import cats.effect.{Clock, IO}
import cats.effect.concurrent.Ref
import lambdaone.kratia.collector.CollectorCRUD.BoxData
import lambdaone.kratia.collector.{BallotMetadata, BinaryProposal, Collector, CollectorCRUD}
import lambdaone.kratia.registry.{Community, Member, Registry, RegistryCRUD}
import lambdaone.toolbox.UniqueGen
import lambdaone.toolbox.mem.CRUDPickInMem
import org.http4s.implicits._
import cats.implicits._
import io.circe.Json
import io.circe.syntax._
import lambdaone.kratia.protocol.MemberData.Nickname
import lambdaone.kratia.protocol.RegistryProtocol.{RegisterRequest, RegisterResponse}
import org.http4s.circe.{jsonEncoderOf, jsonOf, jsonEncoder}
import org.http4s._
import org.http4s.dsl.io._
import io.circe.generic.auto._
import lambdaone.kratia.protocol.CollectorProtocol.{CreateBallotBoxRequest, CreateBallotBoxResponse}
import org.http4s.headers
import org.http4s.server.Router

/**
  * Current unique community 19ce7b9b-a4da-4f9c-9838-c04fcb0ce9db
  */
case class Kratia(
  uniqueGen: UniqueGen[IO, UUID],
  registry: Registry[IO, UUID, MemberData],
  collector: Collector[IO, UUID, BinaryProposal, String]
) {

  val rootCommunity: Community[UUID, MemberData] = Community(UUID.fromString("19ce7b9b-a4da-4f9c-9838-c04fcb0ce9db"))

  def app: HttpApp[IO] = {
    Router("/api/v1" -> (v1registry <+> v1collector)).orNotFound
  }

  def auth(request: Request[IO])(program: (Member[UUID, MemberData], MemberData) => IO[Response[IO]]): IO[Response[IO]] = {
    val Bearer = """^Bearer\s(.*)$""".r
    request.headers.get(headers.Authorization).fold(Forbidden()) { header =>
      header.value match {
        case Bearer(token) =>
          Json.fromString(token).as[UUID].fold[IO[Response[IO]]](_ => Forbidden(), { address =>
            val member = Member[UUID, MemberData](address)
            registry.load(rootCommunity, member).flatMap {
              case None => Forbidden()
              case Some(data) => program(member, data)
            }
          })
        case _ =>
          Forbidden()
      }
    }
  }

  def v1registry: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case request @ POST -> Root / "registry" =>

      implicit val decoder: EntityDecoder[IO, RegisterRequest[UUID, Nickname]] =
        jsonOf[IO, RegisterRequest[UUID, Nickname]]
      implicit val encoder: EntityEncoder[IO, RegisterResponse[UUID, Nickname]] =
        jsonEncoderOf[IO, RegisterResponse[UUID, Nickname]]
      val nicknameRegistry = registry.imap(_.nickname)(MemberData.apply)

      for {
        req <- request.as[RegisterRequest[UUID, Nickname]]
        address <- uniqueGen.gen
        member = Member[UUID, Nickname](address)
        _ <- nicknameRegistry.register(req.community, member, req.data)
        ok <- Ok(RegisterResponse(member))
      } yield ok

    case request @ GET -> Root / "registry" =>
      auth(request) { case (_, data) =>
        Ok(data.nickname.value)
      }
  }

  def v1collector: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case request @ POST -> Root / "collector" =>

      implicit val decoder: EntityDecoder[IO, CreateBallotBoxRequest[BinaryProposal, String]] =
        jsonOf[IO, CreateBallotBoxRequest[BinaryProposal, String]]
      implicit val encoder: EntityEncoder[IO, CreateBallotBoxResponse[UUID, BinaryProposal, String]] =
        jsonEncoderOf[IO, CreateBallotBoxResponse[UUID, BinaryProposal, String]]

      auth(request) { (_, _) =>
        for {
          req <- request.as[CreateBallotBoxRequest[BinaryProposal, String]]
          box <- collector.create(req.validBallot, req.closesOn, req.data)
          ok <- Ok(CreateBallotBoxResponse(BallotMetadata(
            box, req.validBallot, req.closesOn, req.data
          )))
        } yield ok
      }

    case request @ GET -> Root / "collector" =>

      auth(request) { case (_, _) =>
        for {
          list <- collector.list
          ok <- Ok(Json.obj("data" -> list.asJson))
        } yield ok
      }
  }
}

object Kratia {

  def inMem: IO[Kratia] =
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

  def buildInMemCollector: IO[Collector[IO, UUID, BinaryProposal, String]] =
    for {
      store <- Ref.of[IO, Map[UUID, BoxData[UUID, BinaryProposal, String]]](Map.empty)
      clock = Clock.create[IO]
      crud = CRUDPickInMem(store)
      collector = CollectorCRUD[IO, UUID, BinaryProposal, String](clock, crud, UniqueGen.UniqueGenUUID)
    } yield collector
}
