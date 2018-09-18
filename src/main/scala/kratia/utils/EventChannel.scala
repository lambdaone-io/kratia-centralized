package kratia.utils

import cats.implicits._
import cats.effect.{ConcurrentEffect, Effect, Sync}
import fs2.async.mutable.{Queue, Topic}
import io.circe.Json
import kratia.App.ClientConnection
import kratia.Protocol.OutMessage
import kratia.Protocol.ProtocolMessage.KratiaEvent

import scala.concurrent.ExecutionContext

// Not a type class
case class EventChannel[F[_], -E](name: String, channel: Topic[F, OutMessage], payload: E => (String, Json)) {

  def createEvent(event: E): KratiaEvent = {
    val (eventName, body) = payload(event)
    KratiaEvent(name, eventName, body)
  }

  def publish(event: E)(implicit F: Sync[F]): F[Unit] =
    channel.publish1(createEvent(event))

  def subscribe(queue: Queue[F, OutMessage])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): F[Interrupt[F]] =
    runWithInterrupt(channel.subscribe(5).to(queue.enqueue))

  def subscribeFeed(feed: ClientConnection[F])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): F[Unit] =
    for {
      unsubscribe <- subscribe(feed.redirectQueue)
      _ <- feed.subscriptions.update(_ + (name -> unsubscribe))
    } yield ()

  def unsubscribeFeed(feed: ClientConnection[F])(implicit F: Sync[F]): F[Unit] =
    for {
      unsubscribe <- feed.subscriptions.modify { subs =>
        subs.get(name) match {
          case Some(unsub) => (subs - name, unsub)
          case None => (subs, F.unit)
        }
      }
      _ <- unsubscribe
    } yield ()
}

object EventChannel {

  def apply[F[_], E](name: String, initial: E)(payload: E => (String, Json))(implicit F: Effect[F], ec: ExecutionContext): F[EventChannel[F, E]] = {
    val (eventName, body) = payload(initial)
    Topic[F, OutMessage](KratiaEvent(name, eventName, body)).map { channel =>
      new EventChannel[F, E](name, channel, payload)
    }
  }
}
