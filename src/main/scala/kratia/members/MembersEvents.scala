package kratia.members

sealed trait MembersEvents

object MembersEvents {

  case object MembersBoot extends MembersEvents

  case class NewMember(member: Member) extends MembersEvents
}


