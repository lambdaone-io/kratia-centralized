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

class CRUDStoreSqlTest extends FlatSpec with PropertyChecks with Matchers with BeforeAndAfter {

  import EntityGenerators._

  implicit val cs = IO.contextShift(ExecutionContext.global)

  implicit val xa = Transactor
    .fromDriverManager[IO]("org.postgresql.Driver", "jdbc:postgresql:world", "postgres", "")
  //      .fromDriverManager[IO]("org.h2.Driver", "jdbc:h2:mem:")


  val store = CRUDStoreSql.crudPickSqlRegistry[IO, Address, Data]

  before {
    ( CRUDStoreSql.dropTable[IO] *> CRUDStoreSql.createTable[IO] ).unsafeRunSync()
  }

  "Creating members" should "work" in {
    forAll(addressGen, memberGen) { (address: Address, member: Member[Address, Data]) =>
      val result: IO[(Address, Address)] = store.create(member, (address, member.address))
      result.unsafeRunSync shouldBe(address, member.address)
    }
  }


  "Stored members" should "be read back" in {
    forAll(addressGen, memberGen) { (address: Address, member: Member[Address, Data]) =>
      val key = (address, member.address)
      ( store.create(member, key) *> store.get(key) ).unsafeRunSync shouldBe member
    }
  }


}
