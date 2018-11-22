package lambdaone.kratia.influence

import cats.data.State
import cats.implicits._
import lambdaone.kratia.influence.Meritocracy.MeritEndpoint
import lambdaone.kratia.registry.{Community, Member, Registry, RegistryDenotation}
import org.scalatest.FlatSpec

class InfluenceDistributionSpec extends FlatSpec {

  type Address = Int

  type Data = MeritEndpoint

  type Program[A] = RegistryDenotation.Denotation[Address, Data, A]

  val registry: Registry[Program, Address, Data] =
    RegistryDenotation.RegistryDenotation

  val state: Map[(Address, Address), Data] = Map(
    (1, 1) -> MeritEndpoint("a"),
    (1, 2) -> MeritEndpoint("b"),
    (2, 1) -> MeritEndpoint("c"),
    (2, 3) -> MeritEndpoint("d"),
    (2, 4) -> MeritEndpoint("g")
  )

  val merit: Map[MeritEndpoint, Double] = Map(
    MeritEndpoint("a") -> 1.0,
    MeritEndpoint("b") -> 2.0,
    MeritEndpoint("c") -> 1.4,
    MeritEndpoint("d") -> 0.0,
    MeritEndpoint("g") -> 3.0
  )

  val members: List[Member[Address, Data]] = List (
    Member(1),
    Member(2),
    Member(3),
    Member(4)
  )

  val community1: Community[Address, Data] = Community(1)

  val community2: Community[Address, Data] = Community(2)

  def distributeFor[M](community: Community[Address, Data], method: M)(implicit m: InfluenceDistribution[Program, Address, Data, M]): Program[Map[Member[Address, Data], Double]] =
    members
      .traverse[Program, (Member[Address, Data], Double)] { member =>
        InfluenceDistribution.distribute(community, member, method, registry)
          .map(member -> _)
      }
      .map(_.toMap)

  def run[A](p: Program[A]): A =
    RegistryDenotation.run[Address, Data](state)(p)

  def fetchMerit(endpoint: MeritEndpoint): Program[Double] =
    State.pure(merit.getOrElse(endpoint, 0.0))

  "Democracy" should "give same influence to each member" in {

    assert(run(distributeFor(community1, Democracy(1.0))) == Map(
      Member(1) -> 1.0,
      Member(2) -> 1.0,
      Member(3) -> 0.0,
      Member(4) -> 0.0
    ))

    assert(run(distributeFor(community2, Democracy(2.0))) == Map(
      Member(1) -> 2.0,
      Member(2) -> 0.0,
      Member(3) -> 2.0,
      Member(4) -> 2.0
    ))
  }

  "Autocracy" should "give all influence to one member" in {

    assert(run(distributeFor(community1, Autocracy(Member[Address, Data](1), 1.0))) == Map(
      Member(1) -> 1.0,
      Member(2) -> 0.0,
      Member(3) -> 0.0,
      Member(4) -> 0.0
    ))

    assert(run(distributeFor(community2, Autocracy(Member[Address, Data](4), 2.0))) == Map(
      Member(1) -> 0.0,
      Member(2) -> 0.0,
      Member(3) -> 0.0,
      Member(4) -> 2.0
    ))
  }

  "Meritocracy" should "give influence depending on an external service" in {

    assert(run(distributeFor(community1, Meritocracy(fetchMerit))(Meritocracy.meritocraticDistribution[Program, Address])) == Map(
      Member(1) -> 1.0,
      Member(2) -> 2.0,
      Member(3) -> 0.0,
      Member(4) -> 0.0
    ))

    assert(run(distributeFor(community2, Meritocracy(fetchMerit))) == Map(
      Member(1) -> 1.4,
      Member(2) -> 0.0,
      Member(3) -> 0.0,
      Member(4) -> 3.0
    ))
  }
}
