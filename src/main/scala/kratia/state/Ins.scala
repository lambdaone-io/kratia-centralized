package kratia.state

import java.util.UUID

case class Ins[A](id: UUID, model: A)
