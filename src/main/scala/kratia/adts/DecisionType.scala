package kratia.adts

sealed abstract class DecisionType(val typeName: String)

object DecisionType {

  case object DecisionSystemChange

  case object SubCommunity extends DecisionType("")
}
