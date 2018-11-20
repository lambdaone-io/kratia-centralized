package lambdaone.kratia.protocol

import cats.Monad
import cats.implicits._
import lambdaone.kratia.registry.{Community, Member, Registry}
import lambdaone.toolbox.UniqueGen

object Register {

  case class RegisterRequest[A, D](community: Community[A, D], data: D)

  case class RegisterResponse[A, D](member: Member[A, D])

  def register[F[_]: Monad, A, D](request: RegisterRequest[A, D])(implicit registry: Registry[F, A, D], gen: UniqueGen[F, A]): F[RegisterResponse[A, D]] =
    for {
      address <- gen.gen
      member = Member[A, D](address)
      _ <- registry.register(request.community, member, request.data)
    } yield RegisterResponse(member)
}
