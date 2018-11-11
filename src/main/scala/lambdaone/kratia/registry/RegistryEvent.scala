package lambdaone.kratia.registry

sealed trait RegistryEvent

object RegistryEvent {

  case class RegisterMember[A, D](community: Community[A, D], member: Member[A, D], data: D) extends RegistryEvent

  case class UnregisterMember[A, D](community: Community[A, D], member: Member[A, D]) extends RegistryEvent
}
