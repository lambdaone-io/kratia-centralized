package kratia

import kratia.adts._
import kratia.state.{State, Store}

trait Community[F[_]] {

  protected def name: State[F, String]

  protected def domain: State[F, String]

  protected def members: Store[F, (Member, Membership)]

  protected def active: State[F, Map[Address, Collector[F]]]

  protected def createDecision(decision: Decision): F[Address]

  def getName: F[String] = name.get

  def validateMember(member: Member, membership: Membership): F[Boolean] =
    members.exists((member, membership))

  def decide(sender: Member, decision: Decision): F[Address]

  def reportDecision(address: Address, decision: Decision, ballot: Ballot): F[Unit]
}
