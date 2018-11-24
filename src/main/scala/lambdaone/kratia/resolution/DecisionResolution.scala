package lambdaone.kratia.resolution

import lambdaone.kratia.collector.InfluenceAllocation

trait DecisionResolution[F[_], P, M] {

  def resolve(allocation: InfluenceAllocation[P], maxInfluence: Double, method: M): F[Set[P]]

}

object DecisionResolution {

  def resolve[F[_], P, M](allocation: InfluenceAllocation[P], maxInfluence: Double, method: M)(implicit dr: DecisionResolution[F, P, M]): F[Set[P]] =
    dr.resolve(allocation, maxInfluence, method)
}
