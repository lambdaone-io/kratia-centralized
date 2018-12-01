package lambdaone.toolbox.sql

import cats._
import cats.implicits._
import doobie._
import doobie.implicits._
import lambdaone.kratia.registry.Member
import lambdaone.toolbox.CRUDPick

object CRUDStoreSql {

  def crudPickSqlRegistry[F[_] : Monad, A: Read : Put, D](implicit xa: Transactor[F]) = new CRUDPick[F, (A, A), Member[A, D]] {


    override def get(id: (A, A)): F[Option[Member[A, D]]] =
      sql"select member from registry where community = ${id._1} and member = ${id._2}".query[Member[A, D]].option.transact(xa)

    override def update(id: (A, A))(f: Member[A, D] => Member[A, D]): F[Option[Member[A, D]]] = ???

    /** Uses a reference to try to delete the data, returns it if successful */
    override def delete(id: (A, A)): F[Option[Member[A, D]]] = ???

    /** Returns all data within the store */
    override def all: F[Map[(A, A), Member[A, D]]] = ???

    /** Stores `a` with the chosen new unique reference */
    override def create(a: Member[A, D], id: (A, A)): F[(A, A)] = {
      val x: F[Int] = sql"insert into registry (community, member) values (${id._1}, ${id._2})".update.run.transact(xa)
      x.map {
        case 1 => id
      }
    }
  }

  def createTable[F[_] : Monad](implicit xa: Transactor[F]) =
    sql"create table registry (community varchar(255), member varchar(255))".update.run.transact(xa)

  def dropTable[F[_] : Monad](implicit xa: Transactor[F]) =
    sql"drop table registry".update.run.transact(xa)


}
