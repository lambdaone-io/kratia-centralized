package kratia

import org.http4s.Status

trait KratiaFailure extends Throwable {

  def code: Status

  def message: String
}

