package lambdaone.toolbox.sql

import cats._
import cats.implicits._
import doobie._
import doobie.implicits._
import lambdaone.kratia.registry.Member
import lambdaone.toolbox.CRUDPick

object CrudPickSqlRegistry {

  def apply[F[_] : Monad, A: Read : Put, D](implicit xa: Transactor[F]) = new CRUDPick[F, (A, A), Member[A, D]] {

    override def get(id: (A, A)): F[Option[Member[A, D]]] =
      sql"select member from registry where community = ${id._1} and member = ${id._2}".query[Member[A, D]].option.transact(xa)

    // No sensible implementation of update
    override def update(id: (A, A))(f: Member[A, D] => Member[A, D]): F[Option[Member[A, D]]] = ???

    /** Uses a reference to try to delete the data, returns it if successful */
    override def delete(id: (A, A)): F[Option[Member[A, D]]] = sql"delete from registry where community = ${id._1} and member = ${id._2}".update.run
      .transact(xa).map {
      case 1 => Some(Member[A, D](id._2))
      case _ => None
    }

    /** Returns all data within the store */
    override def all: F[Map[(A, A), Member[A, D]]] =
      sql"select community, member from registry".query[(A, A)].to[List]
        .transact(xa)
        .map(l => l.map { case (c, m) => ((c, m), Member[A, D](m)) })
        .map(Map.apply)


    /** Stores `a` with the chosen new unique reference */
    override def create(a: Member[A, D], id: (A, A)): F[(A, A)] = {
      sql"insert into registry (community, member) values (${id._1}, ${id._2})".update.run.transact(xa)
        .map {
          case 1 => id
          // FIXME we can't return the error condition here. Change return type to F[Option[]], or use MonadError?
        }
    }
  }

  def createTable[F[_] : Monad](implicit xa: Transactor[F]) =
    sql"create table registry (community varchar(255), member varchar(255))".update.run.transact(xa)

  def dropTable[F[_] : Monad](implicit xa: Transactor[F]) =
    sql"drop table if exists registry".update.run.transact(xa)


}
