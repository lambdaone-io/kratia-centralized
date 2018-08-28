package kratia.adts

sealed abstract class Proposal(val typeName: String)

object Proposal {

  sealed abstract class Binary(val name: String) extends Proposal("binary")

  object Binary {

    case object Yes extends Binary("yes")

    case object No extends Binary("yes")
  }

  case object RequestForProposal extends Proposal("request_for_proposal")
}
