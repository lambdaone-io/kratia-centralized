package lambdaone.kratia.collector

import io.circe.{Decoder, Encoder, Json}

case class DecisionData(value: Json) extends AnyVal

object DecisionData {

  implicit val circeEncoder: Encoder[DecisionData] =
    Encoder.encodeJson.contramap(_.value)

  implicit val circeDecoder: Decoder[DecisionData] =
    Decoder.decodeJson.map(DecisionData.apply)

}
