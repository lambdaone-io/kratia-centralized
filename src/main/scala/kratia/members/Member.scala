package kratia.members

import cats.Show
import kratia.utils.Address

case class Member(address: Address, nickname: String, secret: Option[Secret])

object Member {

  implicit val showMember: Show[Member] =
    member => s"M(${member.nickname})]"
}


