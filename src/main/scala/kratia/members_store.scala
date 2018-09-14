package kratia

import cats.implicits._
import cats.effect.Effect
import kratia.kratia_app.KratiaFailure
import kratia.kratia_core_model.Member
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

  case object MemberNotFound extends RuntimeException with KratiaFailure {
    override def code: Status = Status.NotFound
    override def message: String = "Member not found."
  }
}
