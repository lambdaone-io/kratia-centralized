package lambdaone.kratia.collector

import cats.data.NonEmptyList
import io.circe.{Decoder, DecodingFailure, Encoder}

sealed abstract class BinaryProposal(val repr: String) {

  override def toString: String = repr
}

object BinaryProposal {

  object Yes extends BinaryProposal("yes")

  object No extends BinaryProposal("no")

  val AllChoices: NonEmptyList[BinaryProposal] = NonEmptyList.fromListUnsafe(List(Yes, No))

  val ballot: Ballot[BinaryProposal] = Ballot[BinaryProposal](BinaryProposal.AllChoices)

  implicit val circeEncoder: Encoder[BinaryProposal] =
    Encoder.instance {
      p => Encoder.encodeString(p.repr)
    }

  implicit def circeDecoder: Decoder[BinaryProposal] =
    Decoder.instance { hcursor =>
      for {
        str <- hcursor.as[String]
        res <- str.toLowerCase match {
          case Yes.repr =>
            Right(Yes)
          case No.repr =>
            Right(No)
          case _ =>
            Left(DecodingFailure(s"$str is not a binary proposal", hcursor.history))
        }
      } yield res
    }
}

