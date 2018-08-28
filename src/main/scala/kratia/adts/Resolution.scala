package kratia.adts

case class Resolution[P](value: Ballot[P] => P) extends AnyVal
