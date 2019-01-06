package lambdaone.kratia.collector

import io.circe.{Decoder, Encoder}

case class DecisionData(value: String) extends AnyVal

object DecisionData {

  implicit val circeEncoder: Encoder[DecisionData] =
    Encoder.encodeString.contramap(_.value)

  implicit val circeDecoder: Decoder[DecisionData] =
    Decoder.decodeString.map(DecisionData.apply)

}
