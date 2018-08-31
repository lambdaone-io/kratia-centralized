package kratia.adts

case class Decision(
  proposal: Proposal,
  description: String,
  domain: String
) {

  type VoteType = proposal.VoteType
}
