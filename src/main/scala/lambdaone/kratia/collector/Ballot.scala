package lambdaone.kratia.collector

import cats.data.NonEmptyList

case class Ballot[P](p: NonEmptyList[P]) extends AnyVal

