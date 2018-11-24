package lambdaone.kratia.collector

import cats.data.NonEmptyList
import io.circe.{Decoder, Encoder}

/**
  *
  * @tparam P proposal type
  */
case class Ballot[P](p: NonEmptyList[P]) extends AnyVal

object Ballot {

  implicit def circeEncoder[P](implicit proposals: Encoder[P]): Encoder[Ballot[P]] =
    Encoder.encodeList[P].contramap[Ballot[P]](_.p.toList)

  implicit def circeDecoder[P](implicit proposals: Decoder[P]): Decoder[Ballot[P]] =
    Decoder.decodeNonEmptyList[P].map(Ballot.apply)
}
