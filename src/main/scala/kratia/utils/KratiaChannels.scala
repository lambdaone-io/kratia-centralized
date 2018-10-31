package kratia.utils

import cats.Functor
import cats.implicits._
import cats.effect.{ConcurrentEffect, Effect, Sync}
import kratia.App.ClientConnection
import kratia.Protocol.ProtocolMessage.KratiaFailure
import lambdaone.toolbox.State

import scala.concurrent.ExecutionContext

case class KratiaChannels[F[_]](channels: State[F, Map[String, EventChannel[F, Nothing]]], log: Logger[F]) {

  def NoSuchChannel(name: String): KratiaFailure = KratiaFailure(400, s"There is no event channel named $name")

  def exists(event: String)(implicit F: Functor[F]): F[Boolean] =
    channels.get.map(_.contains(event))

  def getChannel(name: String)(implicit F: Functor[F]): F[Option[EventChannel[F, Nothing]]] =
    channels.get.map(_.get(name))

  def subscribe(name: String, feed: ClientConnection[F])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): F[Unit] =
    getChannel(name) >>= {
      case Some(channel) =>
        channel.subscribeFeed(feed)
      case None =>
        feed.redirect(NoSuchChannel(name))
    }

  def unsubscribe(name: String, feed: ClientConnection[F])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): F[Unit] =
    getChannel(name) >>= {
      case Some(channel) =>
        channel.unsubscribeFeed(feed)
      case None =>
        feed.redirect(NoSuchChannel(name))
    }

  def registerChannel(channel: EventChannel[F, Nothing])(implicit F: Sync[F]): F[Unit] =
    for {
      alreadyStored <- exists(channel.name)
      _ <-
        if(alreadyStored) log.error(s"Tried to register ${channel.name} but it was already stored")
        else channels.modify { map =>
          (map + (channel.name -> channel)) -> ()
        }
    } yield ()

  def unregisterChannel(channel: EventChannel[F, Nothing])(implicit F: Sync[F]): F[Unit] =
    for {
      alreadyStored <- exists(channel.name)
      _ <-
        if(!alreadyStored) log.error(s"Tried to unregister ${channel.name} but it was not stored")
        else channels.modify { map =>
          (map - channel.name) -> ()
        }
    } yield ()
}

object KratiaChannels {

  def inMem[F[_]](logger: Logger[F])(implicit F: Sync[F]): F[KratiaChannels[F]] =
    State.inMem[F, Map[String, EventChannel[F, Nothing]]](Map.empty).map(store => KratiaChannels(store, logger))
}
