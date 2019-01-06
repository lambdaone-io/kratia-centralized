package lambdaone.kratia.collector

import cats.data.NonEmptyList
import io.circe.{Decoder, Encoder}
import lambdaone.kratia.collector.Proposal.BinaryProposal

case class Ballot(p: NonEmptyList[BinaryProposal]) extends AnyVal

object Ballot {

  implicit def circeEncoder: Encoder[Ballot] =
    Encoder.encodeList[BinaryProposal].contramap[Ballot](_.p.toList)

  implicit def circeDecoder: Decoder[Ballot] =
    Decoder.decodeNonEmptyList[BinaryProposal].map(Ballot.apply)

}
