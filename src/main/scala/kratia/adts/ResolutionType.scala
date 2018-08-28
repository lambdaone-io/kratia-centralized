package kratia.adts

import kratia.utils.Enum

abstract class ResolutionType(val name: String)

object ResolutionType {

  case object Majority extends ResolutionType("majority")

  case object LowInertia extends ResolutionType("low_inertia")

  case object AtLeastOne extends ResolutionType("at_least_one")

  case object Unanimous extends ResolutionType("anonimous")


  implicit object EnumInstance extends Enum[ResolutionType] {

    override def show(a: ResolutionType): String = a.name

    override def lift(s: String): Option[ResolutionType] = all.find(_.name.equalsIgnoreCase(s))

    override def default: ResolutionType = Majority

    override def all: Set[ResolutionType] = Set(
      Majority, LowInertia, AtLeastOne, Unanimous
    )
  }
}
