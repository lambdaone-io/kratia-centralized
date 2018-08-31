package kratia.adts

sealed abstract class Proposal(val typeName: String) {

  type VoteType <: Vote
}

sealed abstract class Vote(val typeName: String)

object Proposal {

  /** Decision System Change */
  case class DSCVote(distribution: InfluenceDistributionType, resolution: DecisionResolutionType) extends Vote("decision_system_change_vote")

  case object DecisionSystemChange extends Proposal("decision_system_change") { type VoteType = DSCVote }

  /** Add member */
  case class AddMemberVote(vote: Boolean) extends Vote("add_member_vote")

  case object AddMember extends Proposal("add_member") { type VoteType = AddMemberVote }
}
