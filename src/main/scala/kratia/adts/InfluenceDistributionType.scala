package kratia.adts

abstract class InfluenceDistributionType(val name: String)

import kratia.utils.Enum

object InfluenceDistributionType {

  case object Autocratic extends InfluenceDistributionType("autocratic")

  case object Oligarchic extends InfluenceDistributionType("oligarchic")

  case object Meritocratic extends InfluenceDistributionType("meritocratic")

  case object Democratic extends InfluenceDistributionType("democratic")

  case object TemporalDegradation extends InfluenceDistributionType("temporal_degradation")

  case object GeolocationBased extends InfluenceDistributionType("geolocation_based")

  implicit object EnumInstance extends Enum[InfluenceDistributionType] {

    override def show(a: InfluenceDistributionType): String = a.name

    override def lift(s: String): Option[InfluenceDistributionType] = all.find(_.name.equalsIgnoreCase(s))

    override def default: InfluenceDistributionType = Democratic

    override def all: Set[InfluenceDistributionType] = Set(
      Autocratic, Oligarchic, Meritocratic, Democratic, TemporalDegradation
    )
  }
}
