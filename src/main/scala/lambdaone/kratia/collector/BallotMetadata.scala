package lambdaone.kratia.collector

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._

case class BallotMetadata(ballotBox: BallotBox, ballot: Ballot, closesOn: Timestamp, data: DecisionData)

object BallotMetadata {

  implicit val circeEncoder: Encoder[BallotMetadata] =
    Encoder.instance { data =>
      Json.obj(
        "ballotBox" -> data.ballotBox.asJson,
        "ballot" -> data.ballot.asJson,
        "closesOn" -> data.closesOn.asJson,
        "data" -> data.data.asJson
      )
    }

  implicit val circeDecoder: Decoder[BallotMetadata] =
    Decoder.instance { hcursor =>
      for {
        ballotBox <- hcursor.downField("ballotBox").as[BallotBox]
        ballot <- hcursor.downField("ballot").as[Ballot]
        closesOn <- hcursor.downField("closesOn").as[Timestamp]
        data <- hcursor.downField("data").as[DecisionData]
      } yield BallotMetadata(ballotBox, ballot, closesOn, data)
    }

}
