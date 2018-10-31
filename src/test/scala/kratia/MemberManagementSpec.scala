package kratia

import kratia.helpers.KratiaSuite
import kratia.Protocol.ProtocolMessage.{KratiaEvent, Register, Registered, Subscribe}
import kratia.members.Member

import scala.concurrent.duration._

class MemberManagementSpec extends KratiaSuite {

  /*
  test("Register user simple") {
    exec { context =>
      for {
        _ <- context ! Register("vledic")
        _ <- context ! Subscribe("members")
        notifications <- context.within(1.second).expect("registration notifications") {
          case Registered(Member(_, "vledic", _)) => true
          case KratiaEvent("members", _, _) => true
          case _ => false
        }
      } yield {
        assert(notifications.length === 3)
      }
    }
  }

  test("Register user notifications 1") {
    exec { context =>
      for {
        _ <- context ! Subscribe("members")
        _ <- context ! Register("vledic")
        _ <- context ! Register("vledic2")
        _ <- context ! Register("vledic3")
        notifications <- context.within(1.second).expect("registration notifications") {
          case KratiaEvent("members", "new_member", _) => true
          case _ => false
        }
      } yield {
        assert(notifications.length === 3)
      }
    }
  }
  */
}
