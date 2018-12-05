package lambdaone.toolbox.sql

import cats._
import cats.implicits._
import doobie._
import doobie.implicits._
import lambdaone.kratia.collector.{BinaryProposal, InfluenceAllocation}
import lambdaone.kratia.collector.CollectorCRUD.{AllVotes, BoxData}
import lambdaone.toolbox.CRUDPick

object CrudPickSqlCollector {

  type BinaryBoxData[A, D] = BoxData[A, BinaryProposal, D]


  def init[F[_] : Monad](implicit xa: Transactor[F]) =
    ( sql"create table ballotbox_binary (id varchar(255), closed_on number)".update.run
      *> sql"create table votes_binary (box varchar(255), member varchar(255), yes number, no number )".update.run
      )
      .transact(xa)

  def tearDown[F[_] : Monad](implicit xa: Transactor[F]) =
    ( sql"drop table if exists ballotbox_binary".update.run
      *> sql"drop table if exists votes_binary".update.run
      ).transact(xa)

  def apply[F[_] : Monad, A: Read : Put, D](implicit xa: Transactor[F], readBox: Read[BinaryBoxData[A, D]], readVotes: Read[AllVotes[A, BinaryProposal]])
  = new CRUDPick[F, A, BoxData[A, BinaryProposal, D]] {
    /** Stores `a` with the chosen new unique reference */
    override def create(a: BoxData[A, BinaryProposal, D], id: A): F[A] =
      sql"insert into ballotbox_binary (id, closes_on) values ($id, ${a.closedOn})".update.run.transact(xa)
        .map {
          case 1 => id
          // FIXME we can't recovers from error here. Change return type to F[Option[]], or use MonadError?
        }

    /** Uses a reference to try to look for the data in the store */
    // FIXME: In one transaction
    override def get(id: A): F[Option[BoxData[A, BinaryProposal, D]]] = {
      val ballotData: F[Option[BoxData[A, BinaryProposal, D]]] = sql"select closed_on, data from ballotbox_binary where id=${id}"
        .query[BoxData[A, BinaryProposal, D]]
        .option.transact(xa)

      ballotData.flatMap { opt =>
        val votes: F[List[(A, Double, Double)]] = sql"select member, yes, no from votes_binary where box = $id"
          .query[(A, Double, Double)].to[List].transact(xa)

        // FIXME proof of vote needed here
        val allVotes: F[AllVotes[A, BinaryProposal]] = votes.map { l =>
          val tuples = l.map { case (a, yes, no) => (a, (a, InfluenceAllocation(Map(BinaryProposal.Yes -> yes, BinaryProposal.No -> no)))) }
          Map(tuples: _*)
        }

        allVotes.map(v => opt.map(bd => bd.copy(votes = v)))
      }

    }

    /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
      * returns the new version if the data if successful */
    override def update(id: A)(f: BoxData[A, BinaryProposal, D] => BoxData[A, BinaryProposal, D])
    : F[Option[BoxData[A, BinaryProposal, D]]] = {
      val before: F[Option[BoxData[A, BinaryProposal, D]]] = get(id)
      before.map { optBoxData =>


      }
    }

    /** Uses a reference to try to delete the data, returns it if successful */
    override def delete(id: A): F[Option[BoxData[A, BinaryProposal, D]]] = ???

    /** Returns all data within the store */
    override def all: F[Map[A, BoxData[A, BinaryProposal, D]]] = ???
  }


}
