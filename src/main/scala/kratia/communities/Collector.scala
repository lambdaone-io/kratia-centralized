package kratia.communities

import java.util.UUID

import cats.Functor
import cats.effect.Sync
import cats.implicits._
import kratia.communities.Collector._
import kratia.Protocol.ProtocolMessage.KratiaFailure
import kratia.communities.Community.{Decision, DecisionAction, Vote}
import kratia.members.Member
import kratia.utils.{Address, State}
import org.http4s.Status

class Collector[F[_]](
    val address: Address,
    val decision: Decision,
    val community: Community[F],
    val action: DecisionAction[F],
    val store: CollectorStore[F]
  ) {

  def vote(member: Member, vote: Vote)(implicit F: Sync[F]): F[ProofOfVote] =
    for {
      _ <- checkIfOpen(member) *> checkIfVoted(member) *> checkIfProposalMatches(vote)
      _ <- store.ballot.update(addToBallot(member.address, vote))
      proof <- genProof(member)
      _ <- store.voted.update((member.address, proof) :: _)
    } yield proof

  def validateVote(proofOfVote: ProofOfVote)(implicit F: Functor[F]): F[Boolean] =
    store.voted.get.map(_.exists(_._2 == proofOfVote))

  def close(implicit F: Sync[F]): F[Unit] =
    for {
      _ <- store.open.set(false)
      _ <- community.reportDecision(this)
    } yield ()

  private def checkIfOpen(member: Member)(implicit F: Sync[F]): F[Unit] =
    store.open.get >>= { isOpen =>
      if (isOpen) F.unit
      else F.raiseError(BallotAlreadyClosed(member))
    }

  private def checkIfVoted(member: Member)(implicit F: Sync[F]): F[Unit] =
    store.voted.get map
      { _.exists(_._1 == member.address) } >>= { voted =>
      if (voted) F.raiseError(MemberAlreadyVoted(member))
      else F.unit
    }

  private def checkIfProposalMatches(vote: Vote)(implicit F: Sync[F]): F[Unit] =
    if(vote.decisionType == decision.decisionType) F.unit
    else F.raiseError(VoteMustBeOfSameProposal)

  private def genProof(member: Member)(implicit F: Sync[F]): F[ProofOfVote] =
    F.delay(ProofOfVote(UUID.randomUUID(), member.nickname))

  private def addToBallot(address: Address, vote: Vote)(ballot: Ballot): Ballot =
    Ballot(ballot.value + (address -> vote))
}

object Collector {

  case class CollectorStore[F[_]] private (
    open: State[F, Boolean],
    ballot: State[F, Ballot],
    voted: State[F, List[(Address, ProofOfVote)]],
  )

  case class Ballot(value: Map[Address, Vote]) extends AnyVal

  case class ProofOfVote(id: UUID, nickname: String)

  /** Failures */

  def CollectorNotFound(address: Address): KratiaFailure = KratiaFailure(Status.NotFound.code, s"Operation on collector ${address.value} couldn't be done because the collector was not found.")

  def BallotAlreadyClosed(member: Member): KratiaFailure = KratiaFailure(Status.BadRequest.code, s"${member.nickname} can't vote on already closed ballot.")

  def MemberAlreadyVoted(member: Member): KratiaFailure = KratiaFailure(Status.BadRequest.code, s"${member.nickname} already voted.")

  val VoteMustBeOfSameProposal: KratiaFailure = KratiaFailure(Status.BadRequest.code, s"Not a valid proposal type, the vote must match the decision proposal type.")

  /** Functions */

  def inMem[F[_]](decision: Decision, community: Community[F], action: DecisionAction[F])(implicit F: Sync[F]): F[Collector[F]] =
    for {
      openState <- State.inMem[F, Boolean](false)
      ballotState <- State.inMem[F, Ballot](Ballot(Map.empty))
      votedState <- State.inMem[F, List[(Address, ProofOfVote)]](List.empty)
      address <- Address.gen[F]
    } yield new Collector[F](address, decision, community, action, CollectorStore(openState, ballotState, votedState))
}
