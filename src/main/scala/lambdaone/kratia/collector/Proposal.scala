package lambdaone.kratia.collector

import cats.data.NonEmptyList
import io.circe.{Decoder, Encoder}

sealed trait Proposal

object Proposal {

  case class BinaryProposal(value: Boolean) extends Proposal

  object BinaryProposal {

    val Yes = BinaryProposal(true)

    val No = BinaryProposal(false)

    val tag: String = "binary-proposal"

    val AllChoices: NonEmptyList[BinaryProposal] = NonEmptyList.fromListUnsafe(List(Yes, No))

    val ballot: Ballot = Ballot(BinaryProposal.AllChoices)

    implicit val circeEncoder: Encoder[BinaryProposal] =
      Encoder.encodeBoolean.contramap(_.value)

    implicit val circeDecoder: Decoder[BinaryProposal] =
      Decoder.decodeBoolean.map(BinaryProposal.apply)

  }

}
