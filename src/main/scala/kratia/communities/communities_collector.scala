package kratia.communities

import java.util.UUID

import cats.Functor
import cats.effect.Sync
import cats.implicits._
import kratia.communities.communities_decision._
import kratia.kratia_core_model.{Address, Community, Member}
import kratia.kratia_protocol.ProtocolMessage.KratiaFailure
import kratia.state.State
import org.http4s.Status

object communities_collector {


  /** Models */

  case class Collector[F[_]](address: Address, decision: Decision, community: Community[F], action: DecisionAction[F], store: CollectorStore[F])

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

  def CollectorInMem[F[_]](decision: Decision, community: Community[F], action: DecisionAction[F])(implicit F: Sync[F]): F[Collector[F]] =
    for {
      openState <- State.inMem[F, Boolean](false)
      ballotState <- State.inMem[F, Ballot](Ballot(Map.empty))
      votedState <- State.inMem[F, List[(Address, ProofOfVote)]](List.empty)
      address <- Address.gen[F]
    } yield Collector[F](address, decision, community, action, CollectorStore(openState, ballotState, votedState))

  def vote[F[_]](member: Member, vote: Vote)(collector: Collector[F])(implicit F: Sync[F]): F[ProofOfVote] =
    for {
      _ <- checkIfOpen(member)(collector) >>= checkIfVoted(member) >>= checkIfProposalMatches(vote)
      _ <- collector.store.ballot.update(addToBallot(member.address, vote))
      proof <- genProof[F](member)
      _ <- collector.store.voted.update((member.address, proof) :: _)
    } yield proof

  def validateVote[F[_]](proofOfVote: ProofOfVote)(collector: Collector[F])(implicit F: Functor[F]): F[Boolean] =
    collector.store.voted.get.map(_.exists(_._2 == proofOfVote))

  def close[F[_]](collector: Collector[F])(implicit F: Sync[F]): F[Unit] =
    for {
      _ <- collector.store.open.set(false)
      _ <- communities_decision.reportDecision(collector)(collector.community)
    } yield ()

  private def checkIfOpen[F[_]](member: Member)(collector: Collector[F])(implicit F: Sync[F]): F[Collector[F]] =
    collector.store.open.get >>= { isOpen =>
      if (isOpen) collector.pure[F]
      else F.raiseError(BallotAlreadyClosed(member))
    }

  private def checkIfVoted[F[_]](member: Member)(collector: Collector[F])(implicit F: Sync[F]): F[Collector[F]] =
    collector.store.voted.get map
      { _.exists(_._1 == member.address) } >>= { voted =>
        if (voted) F.raiseError(MemberAlreadyVoted(member))
        else collector.pure[F]
      }

  private def checkIfProposalMatches[F[_]](vote: Vote)(collector: Collector[F])(implicit F: Sync[F]): F[Collector[F]] =
    if(vote.decisionType == collector.decision.decisionType) collector.pure[F]
    else F.raiseError(VoteMustBeOfSameProposal)

  private def genProof[F[_]](member: Member)(implicit F: Sync[F]): F[ProofOfVote] =
    F.delay(ProofOfVote(UUID.randomUUID(), member.nickname))

  private def addToBallot(address: Address, vote: Vote)(ballot: Ballot): Ballot =
    Ballot(ballot.value + (address -> vote))
}
