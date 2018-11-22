package lambdaone.toolbox

/** CRUD operations based on a type for identifying data (like UUID or a hash) to store some data A
  *
  * @tparam F the wrapping effect
  * @tparam I identification or reference to an instance of A, used to fetch the data and should be unique within the store
  * @tparam A data to be stored and retrieved
  */
trait CRUDPick[F[_], I, A] extends CRUDStore[F, I, A] {

  /** Stores `a` with the chosen new unique reference */
  def create(a: A, id: I): F[I]

}
