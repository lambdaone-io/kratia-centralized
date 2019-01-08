package lambdaone.github.models

import com.github.nscala_time.time.Imports._
import org.joda.time.format.ISODateTimeFormat

case class InstallationAccessToken(
  token: String,
  expires_at: String
) {

  val expiresAt: DateTime =
    ISODateTimeFormat.dateTimeNoMillis().parseDateTime(expires_at)

}

