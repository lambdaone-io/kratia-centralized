package lambdaone.kratia.influence

import lambdaone.kratia.registry.{Community, Member, Registry}
import cats.implicits._
import cats.Functor

sealed trait InfluenceDistribution {

  type Data

  def distribution(member: Member, data: Option[Data]): Double

}

object InfluenceDistribution {

  def apply[F[_]: Functor](community: Community, member: Member, method: InfluenceDistribution)(implicit registry: Registry[F, method.Data]): F[Double] =
    registry.load(community, member).map(data => method.distribution(member, data))

  case class Autocracy(king: Member, base: Double) extends InfluenceDistribution {

    type Data = Unit

    def distribution(member: Member, isPart: Option[Unit]): Double =
      if (isPart.isDefined && member == king) base else 0.0

  }

  case class Democracy(base: Double) extends InfluenceDistribution {

    type Data = Unit

    def distribution(member: Member, isPart: Option[Unit]): Double =
      if (isPart.isDefined) base else 0.0

  }

  case class Merit(value: Double)

  case object Meritocracy extends InfluenceDistribution {

    type Data = Merit

    def distribution(member: Member, isPart: Option[Merit]): Double =
      isPart.fold(0.0)(_.value)

  }
}
