package lambdaone.kratia.collector

trait Collector[F[_]] {

  def create(ballot: Ballot, closesOn: Timestamp, data: DecisionData): F[BallotBox]

  def vote(ballotBox: BallotBox, vote: Vote): F[ProofOfVote]

  def validateVote(ballotBox: BallotBox, proofOfVote: ProofOfVote): F[Boolean]

  def inspect(ballotBox: BallotBox): F[DecisionResults]

  def listOpen: F[List[BallotMetadata]]
}
