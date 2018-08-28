package kratia.adts

case class Ballot(value: Map[Membership, Proposal]) extends AnyVal {

  def add(membership: Membership, proposal: Proposal): Ballot =
    Ballot(value + (membership -> proposal))
}
