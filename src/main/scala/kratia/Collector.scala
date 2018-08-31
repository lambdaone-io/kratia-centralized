package kratia

import java.util.UUID

import cats.effect.{IO, Sync}
import cats.implicits._
import cats.{Functor, Monad, MonadError}
import kratia.state.State
import kratia.Community._
import kratia.Collector._
import org.http4s.Status

trait Collector[F[_]] {

  def address: Address

  def decision: Decision

  def community: Community[F]

  def open: State[F, Boolean]

  def ballot: State[F, Ballot]

  def voted: State[F, List[(Membership, ProofOfVote)]]

  def decisionAction: DecisionAction[F]

  def vote(member: Member, vote: Vote)(implicit F: Sync[F]): F[ProofOfVote] =
    for {
      _ <- checkIfOpen(member)
      _ <- checkIfVoted(member)
      _ <- checkIfProposalMatches(vote)
      _ <- ballot.update(_.add(member.membership, vote))
      proof <- ProofOfVote.gen[F](member)
      _ <- voted.update((member.membership, proof) :: _)
    } yield proof

  def validateVote(proofOfVote: ProofOfVote)(implicit F: Functor[F]): F[Boolean] =
    voted.get.map(_.exists(_._2 == proofOfVote))

  def close(implicit F: Monad[F]): F[Unit] =
    for {
      _ <- open.set(false)
      b <- ballot.get
      _ <- community.reportDecision(address, decision, b)
    } yield ()

  private def checkIfOpen(member: Member)(implicit F: MonadError[F, Throwable]): F[Unit] =
    open.get >>= (o => if(o) F.unit else F.raiseError(BallotAlreadyClosed(member)))

  private def checkIfVoted(member: Member)(implicit F: MonadError[F, Throwable]): F[Unit] =
    voted.get map
      (_.exists(_._1 == member.membership)) >>=
      (voted => if(voted) F.raiseError(MemberAlreadyVoted(member)) else F.unit)

  private def checkIfProposalMatches(vote: Vote)(implicit F: MonadError[F, Throwable]): F[Unit] =
    if(vote.proposal == decision.decisionType) F.unit else F.raiseError(VoteMustBeOfSameProposal)
}

object Collector extends CollectorTypes with CollectorFailures {

  def inMem(dec: Decision, com: Community[IO], act: DecisionAction[IO]): IO[Collector[IO]] =
    for {
      openState <- State.ref[Boolean](false)
      ballotState <- State.ref[Ballot](Ballot(Map.empty))
      votedState <- State.ref[List[(Membership, ProofOfVote)]](List.empty)
      addr <- Address.gen[IO]
    } yield new Collector[IO] {
      override val address: Address = addr
      override val decision: Decision = dec
      override val community: Community[IO] = com
      override val decisionAction: DecisionAction[IO] = act
      override def open: State[IO, Boolean] = openState
      override def ballot: State[IO, Ballot] = ballotState
      override def voted: State[IO, List[(Membership, ProofOfVote)]] = votedState
    }
}

private[kratia] trait CollectorTypes {

  case class Address(value: UUID)
  object Address {
    def gen[F[_]](implicit sync: Sync[F]): F[Address] = sync.delay(Address(UUID.randomUUID()))
  }

  case class Ballot(value: Map[Membership, Vote]) {
    def add(membership: Membership, vote: Vote): Ballot =
      Ballot(value + (membership -> vote))
  }

  case class ProofOfVote(id: UUID, nickname: String)
  object ProofOfVote {
    def gen[F[_]](member: Member)(implicit F: Sync[F]): F[ProofOfVote] =
      F.delay(ProofOfVote(UUID.randomUUID(), member.nickname))
  }
}

private[kratia] trait CollectorFailures { self: CollectorTypes =>

  case class CollectorNotFound(address: Address) extends RuntimeException with KratiaFailure {
    val code: Status = Status.NotFound
    val message: String = s"Operation on collector ${address.value} couldn't be done because the collector was not found."
  }

  case class BallotAlreadyClosed(member: Member) extends RuntimeException with KratiaFailure {
    val code: Status = Status.BadRequest
    val message: String = s"${member.nickname} can't vote on already closed ballot."
  }

  case class MemberAlreadyVoted(member: Member) extends RuntimeException with KratiaFailure {
    val code: Status = Status.BadRequest
    val message: String = s"${member.nickname} already voted."
  }

  case object VoteMustBeOfSameProposal extends RuntimeException with KratiaFailure {
    override def code: Status = Status.BadRequest
    override def message: String = s"Not a valid proposal type, the vote must match the decision proposal type."
  }
}