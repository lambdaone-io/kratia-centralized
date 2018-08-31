package kratia.adts

case class Resolution[R](value: Ballot => R) extends AnyVal
