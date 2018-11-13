package lambdaone.toolbox

import lambdaone.toolbox.denotations.CollectorDenotation
import lambdaone.toolbox.discipline.CollectorTests
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

import cats.instances.string._

class CollectorSpec extends FunSuite with Discipline {

  checkAll("Collector[Denotation[String, ?], Int, String]", CollectorTests(
    CollectorDenotation[Int, String],
    CollectorDenotation.Interpreter[Int, String]
  ).crud)
}
