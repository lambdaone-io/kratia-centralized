package lambdaone.toolbox.denotations

import lambdaone.toolbox.EventStore
import EventStoreDenotation._
import cats.data.State

class EventStoreDenotation[A] extends EventStore[Denotation[A, ?], A] {

  override def emit(event: A): Denotation[A, Unit] =
    State.modify {
      case (events, time) => ((time, event) :: events, time)
    }

  override def listen(callback: A => Denotation[A, Unit]): Denotation[A, Unit] = ???

  override def await(time: Time): Denotation[A, Unit] = ???

  override def load: Denotation[A, List[(Time, A)]] = ???

  override def last: Denotation[A, (Time, A)] = ???
}

object EventStoreDenotation {

  type Time = Long

  type Event[A] = (Time, A)

  type Denotation[A, T] = State[(List[Event[A]], Time), T]
}