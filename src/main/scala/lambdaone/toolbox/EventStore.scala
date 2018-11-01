package lambdaone.toolbox

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

trait EventStore[F[_], A] {

  def append(a: A): F[A]

  def latest: F[A]

  def loadAll: F[NonEmptyList[A]]

  // Scan from earliest to latest
  def find(f: A => Boolean): F[Option[A]]

  def findMap[B](pf: PartialFunction[A, B]): F[Option[B]]

  def filterMap[B](pf: PartialFunction[A, B]): F[List[B]]
}

object EventStore {

  def inMem[F[_], A](first: A)(implicit F: Sync[F]): F[EventStore[F, A]] =
    Ref.of[F, NonEmptyList[A]](NonEmptyList.of(first)).map { ref =>

      new EventStore[F, A] {

        override def append(a: A): F[A] =
          ref.modify(nel => (nel.append(a), a))

        override def latest: F[A] =
          ref.get.map(_.head)

        override def loadAll: F[NonEmptyList[A]] =
          ref.get

        override def find(f: A => Boolean): F[Option[A]] =
          ref.get.map(_.find(f))

        override def findMap[B](pf: PartialFunction[A, B]): F[Option[B]] =
          ref.get.map { nel =>
            nel.foldLeft(None: Option[B]) { (_, a) =>
              if (pf.isDefinedAt(a)) Some(pf(a))
              else None
            }
          }

        override def filterMap[B](pf: PartialFunction[A, B]): F[List[B]] =
          ref.get.map { nel =>
            nel.foldLeft(Nil: List[B]) { (acc, a) =>
              if (pf.isDefinedAt(a)) acc :+ pf(a)
              else acc
            }
          }
      }
    }
}