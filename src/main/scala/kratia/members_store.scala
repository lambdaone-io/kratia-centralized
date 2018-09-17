package kratia

import cats.implicits._
import cats.effect.Effect
import kratia.kratia_core_model.Member
import kratia.kratia_protocol.ProtocolMessage.KratiaFailure
import kratia.state.{Instance, Store}
import org.http4s.Status

import scala.concurrent.ExecutionContext

object members_store {

  /** Models */

  case class MemberStore[F[_]] private (store: Store[F, Member]) extends AnyVal {

    def save(member: Member): F[Instance[Member]] =
      store.create(member)
  }

  object MemberStore {

    def inMem[F[_]](implicit F: Effect[F], ec: ExecutionContext): F[MemberStore[F]] =
      Store.inMem[F, Member].map(MemberStore.apply)
  }


  /** Failures */

  val MemberNotFound: KratiaFailure = KratiaFailure(Status.NotFound.code, "Member not found.")
}
