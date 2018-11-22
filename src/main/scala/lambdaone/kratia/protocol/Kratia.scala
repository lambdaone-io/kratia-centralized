package lambdaone.kratia.protocol

import lambdaone.kratia.registry.Registry

case class Kratia[F[_], A](registry: Registry[F, A, MemberData])

object Kratia {

  def inMem

  implicit def registry[F[_], A](implicit kratia: Kratia[F, A]): Registry[F, A, MemberData] = kratia.registry
}
