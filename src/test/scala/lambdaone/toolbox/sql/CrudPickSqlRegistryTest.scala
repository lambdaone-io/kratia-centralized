package lambdaone.toolbox.sql

import cats.effect.IO
import cats.implicits._
import doobie.Transactor
import lambdaone.kratia.protocol.MemberData
import lambdaone.kratia.protocol.MemberData.Nickname
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.concurrent.ExecutionContext

object EntityGenerators {

  type Address = String
  type Data = String

  val addressGen: Gen[Address] = Gen.uuid.map(_.toString)
  val memberdataGen: Gen[MemberData] = Gen.alphaStr.map(s => MemberData(Nickname(s)))

}

class CrudPickSqlRegistryTest extends FlatSpec with PropertyChecks with Matchers with BeforeAndAfter {

  import EntityGenerators._

  implicit val cs = IO.contextShift(ExecutionContext.global)

  implicit val xa = Transactor
    .fromDriverManager[IO]("org.h2.Driver", "jdbc:h2:mem:kratia;DB_CLOSE_DELAY=-1")

  val store = CrudPickSqlRegistry[IO, Address, Data]

  before {
    ( CrudPickSqlRegistry.dropTable[IO] *> CrudPickSqlRegistry.init[IO] ).unsafeRunSync()
  }

  "Getting non-stored mambers" should "deliver no result" in {
    forAll(addressGen, addressGen) { (community: Address, member: Address) =>
      store.get((community, member)).unsafeRunSync shouldBe None
    }
  }

  "Stored members" should "be available to get" in {
    forAll(addressGen, addressGen, memberdataGen) { (community: Address, member: Address, memberdata: MemberData) =>
      val key = (community, member)
      ( store.create(memberdata, key) *> store.get(key) ).unsafeRunSync shouldBe Some(memberdata)
    }
  }


}
