package lambdaone.kratia.influence

import cats.Functor
import cats.implicits._

case class Democracy(base: Double)

object Democracy {

  def democraticDistribution[F[_]: Functor, A, D]: InfluenceDistribution[F, A, D, Democracy] =
    InfluenceDistribution.lift { (community, member, democracy, registry) =>
      registry.isMember(community, member).map { isPart =>
        if (isPart) democracy.base else 0.0
      }
    }
}



