package kratia.adts

import kratia.utils.Enum

abstract class DecisionResolutionType(val name: String)

object DecisionResolutionType {

  case object Majority extends DecisionResolutionType("majority")

  case object LowInertia extends DecisionResolutionType("low_inertia")

  case object AtLeastOne extends DecisionResolutionType("at_least_one")

  case object Unanimous extends DecisionResolutionType("anonimous")


  implicit object EnumInstance extends Enum[DecisionResolutionType] {

    override def show(a: DecisionResolutionType): String = a.name

    override def lift(s: String): Option[DecisionResolutionType] = all.find(_.name.equalsIgnoreCase(s))

    override def default: DecisionResolutionType = Majority

    override def all: Set[DecisionResolutionType] = Set(
      Majority, LowInertia, AtLeastOne, Unanimous
    )
  }
}
