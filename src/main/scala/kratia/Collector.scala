package kratia

import cats.implicits._
import cats.{Functor, Monad}
import kratia.adts._
import kratia.state.State

trait Collector[F[_]] {

  def address: Address

  def decision: Decision

  def open: State[F, Boolean]

  def ballot: State[F, Ballot]

  def voted: State[F, List[(Membership, ProofOfVote)]]

  def community: Community[F]

  def vote(member: Member, membership: Membership, proposal: Proposal)(implicit F: Monad[F]): F[Option[ProofOfVote]] =
    for {
      isInCommunity <- community.validateMember(member, membership)
      isOpen <- open.get
      alreadyVoted <- voted.get.map(_.exists(_._1 == membership))
      optProof <- {
        if(isInCommunity && isOpen && !alreadyVoted)
          for {
            _ <- ballot.update(_.add(membership, proposal))
            proof <- ProofOfVote.gen[F](member)
            _ <- voted.update((membership, proof) :: _)
          } yield Some(proof)
        else
          F.pure(None)
      }
    } yield optProof

  def checkIfVoted(proofOfVote: ProofOfVote)(implicit F: Functor[F]): F[Boolean] =
    voted.get.map(_.exists(_._2 == proofOfVote))

  def close(implicit F: Monad[F]): F[Unit] =
    for {
      _ <- open.set(false)
      b <- ballot.get
      _ <- community.reportDecision(address, decision, b)
    } yield ()
}
