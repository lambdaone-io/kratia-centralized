package lambdaone.kratia.collector

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._

/**
  *
  * @tparam A address type
  * @tparam P proposal type
  * @tparam D data type
  */
case class BallotMetadata[A, P, D](ballotBox: BallotBox[A, P], ballot: Ballot[P], closesOn: Timestamp, data: D)

object BallotMetadata {

  implicit def circeEncoder[A, P, D](implicit ae: Encoder[A], de: Encoder[D], pe: Encoder[P]): Encoder[BallotMetadata[A, P, D]] =
    Encoder.instance { data =>
      Json.obj(
        "ballotBox" -> data.ballotBox.asJson,
        "ballot" -> data.ballot.asJson,
        "closesOn" -> data.closesOn.asJson,
        "data" -> data.data.asJson
      )
    }

  implicit def circeDecoder[A, P, D](implicit ad: Decoder[A], dd: Decoder[D], pe: Decoder[P]): Decoder[BallotMetadata[A, P, D]] =
    Decoder.instance { hcursor =>
      for {
        ballotBox <- hcursor.downField("ballotBox").as[A]
        ballot <- hcursor.downField("ballot").as[Ballot[P]]
        closesOn <- hcursor.downField("closesOn").as[Timestamp]
        data <- hcursor.downField("data").as[D]
      } yield BallotMetadata[A, P, D](BallotBox(ballotBox), ballot, closesOn, data)
    }
}
