package lambdaone.kratia.influence

import cats.Functor
import cats.implicits._

case class Democracy()

object Democracy {

  def democraticDistribution[F[_]: Functor, A, D]: InfluenceDistribution[F, A, D, Democracy] =
    InfluenceDistribution.lift { (community, member, _, registry) =>
      registry.isMember(community, member).map { isPart =>
        if (isPart) 1.0 else 0.0
      }
    }
}



