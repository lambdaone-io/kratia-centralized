package lambdaone.kratia.collector

import cats.kernel.Monoid
import cats.implicits._

case class InfluenceAllocation[P](value: Map[P, Influence]) extends AnyVal

object InfluenceAllocation {

  implicit def monoid[P]: Monoid[InfluenceAllocation[P]] =
    new Monoid[InfluenceAllocation[P]] {
      override def empty: InfluenceAllocation[P] =
        InfluenceAllocation(Map.empty)
      override def combine(x: InfluenceAllocation[P], y: InfluenceAllocation[P]): InfluenceAllocation[P] =
        InfluenceAllocation(x.value |+| y.value)
    }
}