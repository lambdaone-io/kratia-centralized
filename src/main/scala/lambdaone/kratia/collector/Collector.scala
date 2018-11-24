package lambdaone.kratia.collector

/** Manages ballot boxes and collects votes for such ballot boxes
  *
  * @tparam F context type
  * @tparam A address type
  * @tparam P proposal type
  * @tparam D data type
  */
trait Collector[F[_], A, P, D] {

  def create(ballot: Ballot[P], closesOn: Timestamp, data: D): F[BallotBox[A, P]]

  def vote(ballotBox: BallotBox[A, P], vote: Vote[A, P]): F[ProofOfVote[A]]

  def validateVote(ballotBox: BallotBox[A, P], proofOfVote: ProofOfVote[A]): F[Boolean]

  def inspect(ballotBox: BallotBox[A, P]): F[InfluenceAllocation[P]]

  def list: F[List[BallotMetadata[A, P, D]]]
}
