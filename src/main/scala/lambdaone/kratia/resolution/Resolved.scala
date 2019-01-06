package lambdaone.kratia.resolution

trait Resolved[F[_]] {

  def create(resolution: Resolution): F[Unit]

  def listClosed: F[List[Resolution]]

}
