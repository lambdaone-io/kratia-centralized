package lambdaone.kratia.collector

import io.circe.{Decoder, Encoder}

/** Reference of a ballot box
  *
  * @tparam A address type
  * @tparam P proposal phantom type
  */
case class BallotBox[A, P](address: A) extends AnyVal

object BallotBox {

  implicit def circeEncoder[A, P](implicit address: Encoder[A]): Encoder[BallotBox[A, P]] =
    address.contramap(_.address)

  implicit def circeDecoder[A, P](implicit address: Decoder[A]): Decoder[BallotBox[A, P]] =
    address.map(BallotBox.apply[A, P])
}




