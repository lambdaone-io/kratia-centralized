package lambdaone.kratia.registry

sealed trait RegistryEvent[A]

object RegistryEvent {

  case class RegisterMember[A](community: Community[A], member: Member[A], data: A) extends RegistryEvent[A]
}
