package lambdaone.kratia.registry

import io.circe.{Decoder, Encoder}

case class Community[Address, Data](address: Address) extends AnyVal {

  def map[Data0](f: Data => Data0): Community[Address, Data0] =
    Community(address)
}

object Community {

  implicit def circeEncoder[A, D](implicit address: Encoder[A]): Encoder[Community[A, D]] =
    address.contramap(_.address)

  implicit def circeDecoder[A, D](implicit address: Decoder[A]): Decoder[Community[A, D]] =
    address.map(Community.apply[A, D])
}
