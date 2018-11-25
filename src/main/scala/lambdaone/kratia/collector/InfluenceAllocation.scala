package lambdaone.kratia.collector

import cats.kernel.Monoid
import cats.implicits._
import io.circe.{Decoder, Encoder, Json}

case class InfluenceAllocation[P](value: Map[P, Influence]) extends AnyVal

object InfluenceAllocation {

  implicit def monoid[P]: Monoid[InfluenceAllocation[P]] =
    new Monoid[InfluenceAllocation[P]] {
      override def empty: InfluenceAllocation[P] =
        InfluenceAllocation(Map.empty)
      override def combine(x: InfluenceAllocation[P], y: InfluenceAllocation[P]): InfluenceAllocation[P] =
        InfluenceAllocation(x.value |+| y.value)
    }

  implicit def circeEncoder[P](implicit ep: Encoder[P]): Encoder[InfluenceAllocation[P]] =
    Encoder.instance[InfluenceAllocation[P]] { allocation =>
      Json.fromValues(Encoder.encodeList[(P, Influence)].encodeArray(allocation.value.toList))
    }
  
  implicit def circeDecoder[P](implicit dp: Decoder[P]): Decoder[InfluenceAllocation[P]] =
    Decoder.instance[InfluenceAllocation[P]] { hcursor =>
      Decoder.decodeList[(P, Influence)](Decoder.decodeTuple2[P, Influence])(hcursor).map[InfluenceAllocation[P]] {
        a => InfluenceAllocation(a.toMap)
      }
    }
}