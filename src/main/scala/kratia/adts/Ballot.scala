package kratia.adts

case class Ballot(value: Map[Membership, Vote]) extends AnyVal {

  def add(membership: Membership, vote: Vote): Ballot =
    Ballot(value + (membership -> vote))
}
