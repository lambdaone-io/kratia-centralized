package lambdaone.toolbox

import java.util.UUID

import cats.instances.string._
import lambdaone.toolbox.denotations.CRUDStoreDenotation
import lambdaone.toolbox.discipline.CollectorTests
import lambdaone.toolbox.mem.CRUDStoreInMem
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class CollectorSpec extends FunSuite with Discipline {

  checkAll("Collector[Denotation[String, ?], Int, String]", CollectorTests(
    CRUDStoreDenotation[Int, String],
    CRUDStoreDenotation.Interpreter[Int, String]
  ).crud)
}
