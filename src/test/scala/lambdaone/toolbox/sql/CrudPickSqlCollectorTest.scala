package lambdaone.toolbox.sql

import cats.effect.IO
import cats.implicits._
import doobie.Transactor
import lambdaone.kratia.collector.CollectorCRUD.BoxData
import lambdaone.kratia.collector.{BallotBox, BinaryProposal}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.concurrent.ExecutionContext

object CollectorEntityGenerators {

  type Address = String
  type Data = String

  val addressGen: Gen[Address] = Gen.uuid.map(_.toString)

  val boxDataGen: Gen[BoxData[Address, BinaryProposal, String]] =
    for {
      data <- Gen.alphaStr
      now = System.currentTimeMillis
    } yield BoxData(BinaryProposal.ballot, now + 10000, data, Map())
}

class CrudPickSqlCollectorTest extends FlatSpec with PropertyChecks with Matchers with BeforeAndAfter {

  import CollectorEntityGenerators._

  implicit val cs = IO.contextShift(ExecutionContext.global)

  implicit val xa = Transactor
    .fromDriverManager[IO]("org.h2.Driver", "jdbc:h2:mem:kratia;DB_CLOSE_DELAY=-1")

  val store = CrudPickSqlCollector[IO, Address, BallotBox[Address, BinaryProposal]]

  before {
    ( CrudPickSqlCollector.tearDown[IO] *> CrudPickSqlCollector.init[IO] ).unsafeRunSync()
  }


  "Storing" should "work" in {
    forAll(addressGen, boxDataGen) { (a: Address, boxData: BoxData[Address, BinaryProposal, String]) =>
      store.create(boxData, a) shouldBe Some(1)
    }
  }


}
