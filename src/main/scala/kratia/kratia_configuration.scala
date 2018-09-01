package kratia

import cats.effect.Sync
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.duration.FiniteDuration

object kratia_configuration {


  /** Models */

  case class KratiaConfig(appSpeed: FiniteDuration)


  /** Functions */

  def loadConfig[F[_]](implicit F: Sync[F]): F[KratiaConfig] =
    F.delay {
      val config: Config = ConfigFactory.load()
      KratiaConfig(
        FiniteDuration(config.getDuration("kratia.app-speed").toMillis, "millisecond")
      )
    }
}
