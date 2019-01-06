package lambdaone.kratia.collector

import java.util.UUID

import cats.effect.IO
import lambdaone.kratia.collector.Proposal.BinaryProposal
import lambdaone.kratia.collector.Proposal.BinaryProposal.{Yes, No}
import lambdaone.kratia.protocol.KratiaService
import lambdaone.kratia.registry.Member
import org.scalatest.FlatSpec

class CollectorSpec extends FlatSpec {

  val collector: Collector[IO] =
    KratiaService.buildInMemCollector.unsafeRunSync()

  "Collector" should "simple collect votes" in {
    val program: IO[InfluenceAllocation] = for {
      box <- collector.create(BinaryProposal.ballot, 999999999999l, DecisionData(""))
      member1 = Member(UUID.randomUUID())
      member2 = Member(UUID.randomUUID())
      _ <- collector.vote(box, Vote(member1, InfluenceAllocation(Map(Yes -> 0, No -> 1))))
      _ <- collector.vote(box, Vote(member1, InfluenceAllocation(Map(Yes -> 1, No -> 0))))
      _ <- collector.vote(box, Vote(member2, InfluenceAllocation(Map(Yes -> 1, No -> 0))))
      inspect <- collector.inspect(box)
    } yield inspect

    assert(program.unsafeRunSync().value == Map(Yes -> 2.0, No -> 0.0))
  }
}
