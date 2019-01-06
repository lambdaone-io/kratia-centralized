package lambdaone.toolbox.sql

import cats.effect.IO
import cats.implicits._
import doobie.Transactor
import lambdaone.kratia.collector.BinaryProposal.{No, Yes}
import lambdaone.kratia.collector.CollectorCRUD.BoxData
import lambdaone.kratia.collector.{BinaryProposal, InfluenceAllocation}
import org.scalacheck.{Gen, Shrink, ShrinkLowPriority}
import org.scalacheck.Gen.choose
import org.scalatest.prop.PropertyChecks
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import org.scalacheck.Prop._

import scala.concurrent.ExecutionContext

object CollectorEntityGenerators {

  type Address = String
  type Data = String

  val addressGen: Gen[Address] = Gen.uuid.map(_.toString)

  val emptyBoxDataGen: Gen[BoxData[Address, BinaryProposal, String]] =
    for {
      data <- Gen.alphaStr
      now = System.currentTimeMillis
    } yield BoxData(BinaryProposal.ballot, now + 10000, data, Map())

  val boxDataWithOneVoteGen: Gen[BoxData[Address, BinaryProposal, String]] =
    for {
      emptyBox <- emptyBoxDataGen
      member <- addressGen
      proofOfVote <- addressGen
      yesInfluence <- choose(0.0, 1.0)
    } yield
      emptyBox.copy(
        votes = Map(
          proofOfVote -> (member, InfluenceAllocation(
            Map(Yes -> yesInfluence, No -> (1.0 - yesInfluence))
          ))
        )
      )

  val boxDataWithTwoVotesGen: Gen[BoxData[Address, BinaryProposal, String]] =
    for {
      boxDataWithOneVote <- boxDataWithOneVoteGen
      member <- addressGen
      proofOfVote <- addressGen
      yesInfluence <- choose(0.0, 1.0)
    } yield
      boxDataWithOneVote.copy(
        votes = boxDataWithOneVote.votes ++ Set(
          proofOfVote -> (member, InfluenceAllocation(
            Map(Yes -> yesInfluence, No -> (1.0 - yesInfluence))
          ))
        )
      )
}

class CrudPickSqlCollectorTest
    extends FlatSpec
    with PropertyChecks
    with Matchers
    with BeforeAndAfter
    with ShrinkLowPriority {

  import CollectorEntityGenerators._

  implicit val cs = IO.contextShift(ExecutionContext.global)

  implicit val xa = Transactor
    .fromDriverManager[IO](
      "org.h2.Driver",
      "jdbc:h2:mem:kratia;DB_CLOSE_DELAY=-1"
    )

  val store = CrudPickSqlCollector[IO, Address, String]

  before {
    (CrudPickSqlCollector.tearDown[IO] *> CrudPickSqlCollector.init[IO])
      .unsafeRunSync()
  }

  "Storing" should "work" in {
    forAll(addressGen, emptyBoxDataGen) {
      (a: Address, boxData: BoxData[Address, BinaryProposal, String]) =>
        store.create(boxData, a).unsafeRunSync() shouldBe a
    }
  }

  "Adding votes" should "work" in {
    def addVote(boxData: BoxData[Address, BinaryProposal, String],
                voter: Address,
                proof: Address) =
      boxData.copy(
        votes = boxData.votes + (
          (
            voter,
            (proof, InfluenceAllocation(Map(Yes -> 1.0, No -> 0.0)))
          )
        )
      )

    forAll(addressGen, addressGen, addressGen, boxDataWithOneVoteGen) {
      (a: Address,
       voter: Address,
       proof: Address,
       boxData: BoxData[Address, BinaryProposal, String]) =>
        (store.create(boxData, a) *>
          store.update(a)(bd => addVote(bd, voter, proof)))
          .unsafeRunSync() shouldBe Some(addVote(boxData, voter, proof))
    }
  }

  "Stored ballot box without votes" should "be read back OK" in {
    forAll(addressGen, emptyBoxDataGen) { (a, boxData) =>
      (store.create(boxData, a) *>
        store.get(a)).unsafeRunSync() shouldBe Some(boxData)
    }
  }

  "Stored 2 ballot boxes without votes" should "be read back OK" in {
    forAll(addressGen, emptyBoxDataGen, addressGen, emptyBoxDataGen) {

      (a1, boxData1, a2, boxData2) =>
        val actual =
          (CrudPickSqlCollector.tearDown[IO] *> CrudPickSqlCollector.init[IO] *>
            store.create(boxData1, a1) *>
            store.create(boxData2, a2) *>
            store.all).unsafeRunSync()
        val expected = Map(a1 -> boxData1, a2 -> boxData2)
        actual shouldBe expected

    }
  }

  "Stored ballot box without votes" should "be gone after deletion" in {
    forAll(addressGen, emptyBoxDataGen) { (a, boxData) =>
      (store.create(boxData, a) *>
        store.delete(a) *>
        store.get(a)).unsafeRunSync() shouldBe None
    }
  }

  "Stored ballot box with 1 vote" should "be read back OK" in {
    forAll(addressGen, boxDataWithOneVoteGen) { (a, boxData) =>
      (store.create(boxData, a) *>
        store.get(a)).unsafeRunSync() shouldBe Some(boxData)
    }
  }

  "Stored ballot box with 2 votes" should "be read back OK" in {
    forAll(addressGen, boxDataWithTwoVotesGen) { (a, boxData) =>
      (store.create(boxData, a) *>
        store.get(a)).unsafeRunSync() shouldBe Some(boxData)
    }
  }

}
