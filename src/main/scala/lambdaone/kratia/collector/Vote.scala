package lambdaone.kratia.collector

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import lambdaone.kratia.registry.Member

case class Vote(member: Member, influenceAllocation: InfluenceAllocation)

object Vote {

  implicit val circeEncoder: Encoder[Vote] =
    Encoder { vote =>
      Json.obj(
        "member" -> vote.member.asJson,
        "influenceAllocation" -> vote.influenceAllocation.asJson
      )
    }

  implicit val circeDecoder: Decoder[Vote] =
    Decoder { hcursor =>
      for {
        member <- hcursor.downField("member").as[Member]
        alloc <- hcursor.downField("influenceAllocation").as[InfluenceAllocation]
      } yield Vote(member, alloc)
    }

}
