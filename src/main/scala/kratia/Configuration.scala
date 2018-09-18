package kratia

import cats.effect.Sync
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.duration.FiniteDuration

case class Configuration(appSpeed: FiniteDuration)

object Configuration {

  def loadConfig[F[_]](implicit F: Sync[F]): F[Configuration] =
    F.delay {
      val config: Config = ConfigFactory.load()
      Configuration(
        FiniteDuration(config.getDuration("kratia.app-speed").toMillis, "millisecond")
      )
    }
}
