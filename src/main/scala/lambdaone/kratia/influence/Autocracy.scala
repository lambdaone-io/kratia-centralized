package lambdaone.kratia.influence

import cats.Functor
import cats.implicits._
import lambdaone.kratia.registry.Member

case class Autocracy[A, D](king: Member[A, D], base: Double)

object Autocracy {

  def autocraticDistribution[F[_]: Functor, A, D]: InfluenceDistribution[F, A, D, Autocracy[A, D]] =
    InfluenceDistribution.lift { (community, member, autocracy, registry) =>
      registry.isMember(community, member).map { isPart =>
        if (isPart && member.address == autocracy.king.address) autocracy.base else 0.0
      }
    }
}
