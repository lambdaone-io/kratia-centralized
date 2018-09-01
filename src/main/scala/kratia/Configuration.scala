package kratia

import scala.concurrent.duration.FiniteDuration

object Configuration {

  case class KratiaConfig(appSpeed: FiniteDuration)

}
