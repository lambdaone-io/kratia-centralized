package lambdaone.kratia.influence

import lambdaone.kratia.registry.{Community, Member, Registry}
import InfluenceDistribution._
import cats.{Functor, Monad}
import cats.implicits._

trait InfluenceDistribution[F[_], Data, Method] {

  def dist(community: Community[Data], member: Member[Data], method: Method)(implicit registry: Registry[F, Data]): F[Double]
}

object InfluenceDistribution {

  def apply[F[_], D, M](community: Community[D], member: Member[D], method: M)(implicit inf: InfluenceDistribution[F, D, M], registry: Registry[F, D]): F[Double] =
    inf.dist(community, member, method)

  case class Democracy()

  implicit def democratic[F[_]: Functor, D]: InfluenceDistribution[F, D, Democracy] =
    new Democratic[F, D]

  case class Autocracy[D](king: Member[D])

  implicit def autocratic[F[_]: Functor, D]: InfluenceDistribution[F, D, Autocracy[D]] =
    new Autocratic[F, D]

  case class MeritAddress(value: String) extends AnyVal

  case class Meritocracy[F[_]](query: MeritAddress => F[Double])

  implicit def meritocratic[F[_]: Monad]: InfluenceDistribution[F, MeritAddress, Meritocracy[F]] =
    new Meritocratic[F]
}

class Democratic[F[_]: Functor, D] extends InfluenceDistribution[F, D, Democracy] {

  def dist(community: Community[D], member: Member[D], method: Democracy)(implicit registry: Registry[F, D]): F[Double] =
    registry.isMember(community, member).map { isPart =>
      if (isPart) 1.0 else 0.0
    }
}

class Autocratic[F[_]: Functor, D] extends InfluenceDistribution[F, D, Autocracy[D]] {

  def dist(community: Community[D], member: Member[D], method: Autocracy[D])(implicit registry: Registry[F, D]): F[Double] =
    registry.isMember(community, member).map { isPart =>
      if (isPart && member.address == method.king.address) 1.0 else 0.0
    }
}

class Meritocratic[F[_]: Monad] extends InfluenceDistribution[F, MeritAddress, Meritocracy[F]] {

  def dist(community: Community[MeritAddress], member: Member[MeritAddress], method: Meritocracy[F])(implicit registry: Registry[F, MeritAddress]): F[Double] =
    registry.load(community, member).flatMap[Double] {
      case Some(address) => method.query(address)
      case None => 0.0.pure[F]
    }
}