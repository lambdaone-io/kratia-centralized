package lambdaone.toolbox.sql

import cats._
import doobie._
import doobie.implicits._
import lambdaone.kratia.collector.CollectorCRUD.BoxData
import lambdaone.toolbox.CRUDPick

object CrudPickSqlCollector {

  def createTable[F[_] : Monad](implicit xa: Transactor[F]) =
    sql"create table collector (community varchar(255), member varchar(255))".update.run.transact(xa)

  def dropTable[F[_] : Monad](implicit xa: Transactor[F]) =
    sql"drop table if exists collector".update.run.transact(xa)

  def apply[F[_] : Monad, A: Read : Put, P, D](implicit xa: Transactor[F])
  = new CRUDPick[F, A, BoxData[A, P, D]] {
    /** Stores `a` with the chosen new unique reference */
    override def create(a: BoxData[A, P, D], id: A): F[A] = ???

    /** Uses a reference to try to look for the data in the store */
    override def get(id: A): F[Option[BoxData[A, P, D]]] = ???

    /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
      * returns the new version if the data if successful */
    override def update(id: A)(f: BoxData[A, P, D] => BoxData[A, P, D]): F[Option[BoxData[A, P, D]]] = ???

    /** Uses a reference to try to delete the data, returns it if successful */
    override def delete(id: A): F[Option[BoxData[A, P, D]]] = ???

    /** Returns all data within the store */
    override def all: F[Map[A, BoxData[A, P, D]]] = ???
  }


}
