package lambdaone.toolbox.sql

import cats._
import cats.implicits._
import doobie._
import doobie.implicits._
import lambdaone.kratia.protocol.MemberData
import lambdaone.kratia.protocol.MemberData.Nickname
import lambdaone.toolbox.CRUDPick

object CrudPickSqlRegistry {

  def init[F[_] : Monad](implicit xa: Transactor[F]): F[Int] =
    sql"create table registry (community varchar(255), member varchar(255), nickname varchar(255))".update.run.transact(xa)

  def dropTable[F[_] : Monad](implicit xa: Transactor[F]): F[Int] =
    sql"drop table if exists registry".update.run.transact(xa)

  def apply[F[_] : Monad, A: Read : Put, D]
  (implicit xa: Transactor[F])
  : CRUDPick[F, (A, A), MemberData] = new CRUDPick[F, (A, A), MemberData] {

    override def get(id: (A, A)): F[Option[MemberData]] =
      sql"select nickname from registry where community = ${id._1} and member = ${id._2}".query[String]
        .option.transact(xa)
        .map(_.map(s => MemberData(Nickname(s))))

    override def update(id: (A, A))(f: MemberData => MemberData): F[Option[MemberData]] = ???

    /** Uses a reference to try to delete the data, returns it if successful */
    override def delete(id: (A, A)): F[Option[MemberData]] = sql"delete from registry where community = ${id._1} and member = ${id._2}".update.run
      .transact(xa).map {
      case 1 => None // FIXME. Maybe we should change the `delete` signature
      case _ => None
    }

    /** Returns all data within the store */
    override def all: F[Map[(A, A), MemberData]] =
      sql"select community, member, nickname from registry".query[(A, A, String)].to[List]
        .transact(xa)
        .map(l => l.map { case (c, m, nickname) => ((c, m), MemberData(Nickname(nickname))) })
        .map(l => Map[(A, A), MemberData](l: _*))


    /** Stores `a` with the chosen new unique reference */
    override def create(a: MemberData, id: (A, A)): F[(A, A)] = {
      sql"insert into registry (community, member, nickname) values (${id._1}, ${id._2}, ${a.nickname})".update.run.transact(xa)
        .map {
          case 1 => id
          // FIXME we can't recovers from error here. Change return type to F[Option[]], or use MonadError?
        }
    }
  }




}
