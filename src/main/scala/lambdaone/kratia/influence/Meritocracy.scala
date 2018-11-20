package lambdaone.kratia.influence

import cats.Monad
import cats.implicits._

case class Meritocracy[F[_]](query: Meritocracy.MeritEndpoint => F[Double])

object Meritocracy {

  case class MeritEndpoint(value: String) extends AnyVal

  def meritocraticDistribution[F[_]: Monad, A]: InfluenceDistribution[F, A, MeritEndpoint, Meritocracy[F]] =
    InfluenceDistribution.lift { (community, member, meritocracy, registry) =>
      registry.load(community, member).flatMap[Double] {
        case Some(address) => meritocracy.query(address)
        case None => 0.0.pure[F]
      }
    }
}
