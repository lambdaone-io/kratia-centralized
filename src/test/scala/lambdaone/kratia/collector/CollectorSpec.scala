package lambdaone.kratia.collector

import java.util.UUID

import cats.effect.IO
import lambdaone.kratia.collector.BinaryProposal.{No, Yes}
import lambdaone.kratia.protocol.{KratiaInMem}
import org.scalatest.FlatSpec

class CollectorSpec extends FlatSpec {

  val collector: Collector[IO, UUID, BinaryProposal, String] =
    KratiaInMem.buildInMemCollector.unsafeRunSync()

  "Collector" should "simple collect votes" in {
    val program: IO[InfluenceAllocation[BinaryProposal]] = for {
      box <- collector.create(BinaryProposal.ballot, closesOn = 999999999999l, "")
      member1 = UUID.randomUUID()
      member2 = UUID.randomUUID()
      _ <- collector.vote(box, Vote(member1, InfluenceAllocation(Map(Yes -> 0, No -> 1))))
      _ <- collector.vote(box, Vote(member1, InfluenceAllocation(Map(Yes -> 1, No -> 0))))
      _ <- collector.vote(box, Vote(member2, InfluenceAllocation(Map(Yes -> 1, No -> 0))))
      inspect <- collector.inspect(box)
    } yield inspect

    assert(program.unsafeRunSync().value == Map(Yes -> 2.0, No -> 0.0))
  }
}
