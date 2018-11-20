package lambdaone.kratia.influence

import lambdaone.kratia.registry.{Community, Member, Registry}
import cats.{Functor, Monad}
import lambdaone.kratia.influence.Meritocracy.MeritEndpoint

trait InfluenceDistribution[F[_], Address, Data, Method] {

  def dist(community: Community[Address, Data], member: Member[Address, Data], method: Method, registry: Registry[F, Address, Data]): F[Double]
}

object InfluenceDistribution {

  def distribute[F[_], A, D, M](community: Community[A, D], member: Member[A, D], method: M, registry: Registry[F, A, D])(implicit inf: InfluenceDistribution[F, A, D, M]): F[Double] =
    inf.dist(community, member, method, registry)

  def lift[F[_], A, D, M](f: (Community[A, D], Member[A, D], M, Registry[F, A, D]) => F[Double]): InfluenceDistribution[F, A, D, M] =
    (community: Community[A, D], member: Member[A, D], method: M, registry: Registry[F, A, D]) => f(community, member, method, registry)

  implicit def democratic[F[_]: Functor, A, D]: InfluenceDistribution[F, A, D, Democracy] =
    Democracy.democraticDistribution

  implicit def autocratic[F[_]: Functor, A, D]: InfluenceDistribution[F, A, D, Autocracy[A, D]] =
    Autocracy.autocraticDistribution

  implicit def meritocratic[F[_]: Monad, A]: InfluenceDistribution[F, A, MeritEndpoint, Meritocracy[F]] =
    Meritocracy.meritocraticDistribution
}
