package lambdaone.toolbox.sql

import cats._
import cats.implicits._
import doobie._
import doobie.implicits._
import lambdaone.kratia.collector.CollectorCRUD.{AllVotes, BoxData}
import lambdaone.kratia.collector.{
  BallotBox,
  BinaryProposal,
  InfluenceAllocation,
  Timestamp
}
import lambdaone.toolbox.CRUDPick

object CrudPickSqlCollector {

  type BinaryBoxData[A, D] = BoxData[A, BinaryProposal, D]

  def init[F[_]: Monad](implicit xa: Transactor[F]) =
    (sql"create table ballotbox_binary (id varchar(255), data varchar(255), closes_on number)".update.run
      *> sql"create table votes_binary (box varchar(255), member varchar(255), proof varchar(255), yes number, no number )".update.run)
      .transact(xa)

  def tearDown[F[_]: Monad](implicit xa: Transactor[F]) =
    (sql"drop table if exists ballotbox_binary".update.run
      *> sql"drop table if exists votes_binary".update.run).transact(xa)

  def apply[F[_]: Monad, A: Read: Put, D: Read: Put](
    implicit xa: Transactor[F]
  ) = new CRUDPick[F, A, BoxData[A, BinaryProposal, D]] {

    /** Stores `a` with the chosen new unique reference */
    override def create(boxData: BoxData[A, BinaryProposal, D], id: A): F[A] = {
      sql"insert into ballotbox_binary (id, data, closes_on) values ($id, ${boxData.data}, ${boxData.closedOn})".update.run
        .flatMap {
          case 1 =>
            boxData.votes
              .map {
                case (member, (proof, InfluenceAllocation(ia))) =>
                  val yes: Double = ia.get(BinaryProposal.Yes).getOrElse(0.0)
                  val no: Double = ia.get(BinaryProposal.No).getOrElse(0.0)
                  val y: doobie.ConnectionIO[Int] =
                    sql"insert into votes_binary (box, member, proof, yes, no) values ($id, ${member}, ${proof}, ${yes}, ${no})".update.run
                  // FIXME we can't recovers from error here. Change return type to F[Option[]], or use MonadError?
                  y
              }
              .toList
              .sequence

        }
        .transact(xa)
        .map { _ => id
        }
    }

    /** Uses a reference to try to look for the data in the store */
    // FIXME: In one transaction
    override def get(id: A): F[Option[BoxData[A, BinaryProposal, D]]] = {
      val ballotData: F[Option[BoxData[A, BinaryProposal, D]]] =
        sql"select closes_on, data from ballotbox_binary where id=${id}"
          .query[(Timestamp, D)]
          .map {
            case (ts, d) =>
              BoxData[A, BinaryProposal, D](BinaryProposal.ballot, ts, d, Map())
          }
          .option
          .transact(xa)

      ballotData.flatMap { opt =>
        val votes: F[List[(A, A, Double, Double)]] =
          sql"select member, proof, yes, no from votes_binary where box = $id"
            .query[(A, A, Double, Double)]
            .to[List]
            .transact(xa)

        val allVotes: F[AllVotes[A, BinaryProposal]] = votes.map { l =>
          val tuples = l.map {
            case (a, proof, yes, no) =>
              (
                a,
                (
                  proof,
                  InfluenceAllocation(
                    Map(BinaryProposal.Yes -> yes, BinaryProposal.No -> no)
                  )
                )
              )
          }
          Map(tuples: _*)
        }
        allVotes.map(v => opt.map(bd => bd.copy(votes = v)))
      }

    }

    /**
      * This looks awful
      */
    override def update(id: A)(
      f: BoxData[A, BinaryProposal, D] => BoxData[A, BinaryProposal, D]
    ): F[Option[BoxData[A, BinaryProposal, D]]] = {
      val before: F[Option[BoxData[A, BinaryProposal, D]]] = get(id)
      val after: F[Option[BoxData[A, BinaryProposal, D]]] = before.map(_.map(f))

      val xx: F[Option[F[List[Int]]]] = (before, after).mapN { (optB, optA) =>
        (optB, optA).mapN { (b, a) =>
          val addedVotes
            : Set[(A, (A, InfluenceAllocation[BinaryProposal]))] = a.votes.toSet diff b.votes.toSet
          val removedVotes = b.votes.toSet diff a.votes.toSet

          val added: Set[doobie.ConnectionIO[Int]] = addedVotes.map {
            case (member, (proof, InfluenceAllocation(ia))) =>
              val yes: Double = ia.get(BinaryProposal.Yes).getOrElse(0.0)
              val no: Double = ia.get(BinaryProposal.No).getOrElse(0.0)
              sql"insert into votes_binary (box, member, proof, yes, no) values ($id, ${member}, ${proof}, ${yes}, ${no})".update.run
          }
          val deleted: Set[doobie.ConnectionIO[Int]] = removedVotes.map {
            case (member, _) =>
              sql"delete from votes_binarywhere box = $id, and member = ${member}".update.run
          }

          (added ++ deleted).toList.sequence.transact(xa)
        }
      }

      val ffList: F[F[List[Int]]] = xx.map(_.getOrElse(Monad[F].pure(List())))
      val fList: F[List[Int]] = ffList.flatMap { i => i
      }
      fList.flatMap { _ => after
      }

    }

    /** Uses a reference to try to delete the data, returns it if successful */
    override def delete(id: A): F[Option[BoxData[A, BinaryProposal, D]]] =
      (sql"delete from ballotbox_binary where id = $id".update.run
        *> sql"delete from votes_binary where box = $id".update.run)
        .transact(xa)
        .map(_ => None) // FIXME: returning Option[A] is costly and probably not necessary; reiew the API

    /** Returns all data within the store */
    override def all: F[Map[A, BoxData[A, BinaryProposal, D]]] = {

      sql"select id, closes_on, data from ballotbox_binary "
        .query[(A, Timestamp, D)]
        .map {
          case (id, ts, d) =>
            (
              id,
              BoxData[A, BinaryProposal, D](BinaryProposal.ballot, ts, d, Map())
            )
        }
        .to[List]
        .transact(xa)
        .map(_.toMap)

    }
  }

}
