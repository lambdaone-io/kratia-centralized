package lambdaone.kratia.registry

import io.circe.{Decoder, Encoder}

/**
  *
  * @tparam A address type
  * @tparam D data phantom type
  */
case class Community[A, D](address: A) extends AnyVal {

  def map[D0](f: D => D0): Community[A, D0] =
    Community(address)
}

object Community {

  implicit def circeEncoder[A, D](implicit address: Encoder[A]): Encoder[Community[A, D]] =
    address.contramap(_.address)

  implicit def circeDecoder[A, D](implicit address: Decoder[A]): Decoder[Community[A, D]] =
    address.map(Community.apply[A, D])
}
