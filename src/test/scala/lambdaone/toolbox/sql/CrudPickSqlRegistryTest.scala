package lambdaone.toolbox.sql

import cats.effect.IO
import cats.implicits._
import doobie.Transactor
import lambdaone.kratia.registry.Member
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.concurrent.ExecutionContext

object EntityGenerators {

  type Address = String
  type Data = String

  val addressGen: Gen[Address] = Gen.uuid.map(_.toString)
  val memberGen: Gen[Member[Address, Data]] = addressGen.map(Member(_))

}

class CrudPickSqlRegistryTest extends FlatSpec with PropertyChecks with Matchers with BeforeAndAfter {

  import EntityGenerators._

  implicit val cs = IO.contextShift(ExecutionContext.global)

  implicit val xa = Transactor
    .fromDriverManager[IO]("org.h2.Driver", "jdbc:h2:mem:kratia;DB_CLOSE_DELAY=-1")

  val store = CrudPickSqlRegistry[IO, Address, Data]

  before {
    ( CrudPickSqlRegistry.dropTable[IO] *> CrudPickSqlRegistry.createTable[IO] ).unsafeRunSync()
  }

  "Getting non-stored mambers" should "deliver no result" in {
    forAll(addressGen, memberGen) { (address: Address, member: Member[Address, Data]) =>
      store.get((address, member.address)).unsafeRunSync shouldBe None
    }
  }

  "Stored members" should "be available to get" in {
    forAll(addressGen, memberGen) { (address: Address, member: Member[Address, Data]) =>
      val key = (address, member.address)
      ( store.create(member, key) *> store.get(key) ).unsafeRunSync shouldBe Some(member)
    }
  }


}
