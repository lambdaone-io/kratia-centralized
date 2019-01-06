package lambdaone.kratia.collector

import cats.implicits._
import cats.kernel.Monoid
import io.circe.{Decoder, Encoder}
import lambdaone.kratia.collector.Proposal.BinaryProposal

case class InfluenceAllocation(value: Map[BinaryProposal, Influence])

object InfluenceAllocation {

  implicit def monoid: Monoid[InfluenceAllocation] = {
    new Monoid[InfluenceAllocation] {
      override def empty: InfluenceAllocation = InfluenceAllocation(Map.empty)
      override def combine(x: InfluenceAllocation, y: InfluenceAllocation): InfluenceAllocation =
        InfluenceAllocation(x.value |+| y.value)
    }
  }

  implicit def circeEncoder: Encoder[InfluenceAllocation] =
    Encoder.encodeMap[String, Double].contramap[InfluenceAllocation] { alloc =>
      alloc.value.map { case (proposal, inf) =>
        if (proposal.value) ("yes", inf)
        else ("no", inf)
      }
    }

  implicit def circeDecoder: Decoder[InfluenceAllocation] =
    Decoder.decodeMap[String, Double].map { m =>
      InfluenceAllocation(m.map {
        case ("yes", inf) => (BinaryProposal(true), inf)
        case (_, inf) => (BinaryProposal(false), inf)
      })
    }

}