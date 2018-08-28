package kratia.adts

import java.util.UUID

import cats.effect.Sync

case class ProofOfVote(id: UUID, nickname: String)

object ProofOfVote {

  def gen[F[_]](member: Member)(implicit F: Sync[F]): F[ProofOfVote] =
    F.delay(ProofOfVote(UUID.randomUUID(), member.nickname))
}
