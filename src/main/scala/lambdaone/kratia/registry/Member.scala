package lambdaone.kratia.registry

import io.circe.{Decoder, Encoder}

case class Member[Address, Data](address: Address) extends AnyVal {

  def map[Data0](f: Data => Data0): Member[Address, Data0] =
    Member(address)
}

object Member {

  implicit def circeEncoder[A, D](implicit address: Encoder[A]): Encoder[Member[A, D]] =
    address.contramap(_.address)

  implicit def circeDecoder[A, D](implicit address: Decoder[A]): Decoder[Member[A, D]] =
    address.map(Member.apply[A, D])
}
