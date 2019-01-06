package lambdaone.kratia.collector

import java.util.UUID

import io.circe.{Decoder, Encoder}

case class BallotBox(address: UUID) extends AnyVal

object BallotBox {

  implicit def circeEncoder: Encoder[BallotBox] =
    Encoder.encodeUUID.contramap(_.address)

  implicit def circeDecoder: Decoder[BallotBox] =
    Decoder.decodeUUID.map(BallotBox.apply)

}




