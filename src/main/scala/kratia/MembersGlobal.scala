package kratia

import kratia.state.Store
import kratia.Community.{Member, Secret}

trait MembersGlobal[F[_]] {

  def members: Store[F, Member]

  def create(nickname: String): F[Secret]

  def authenticate(secret: String): F[Member]
}
