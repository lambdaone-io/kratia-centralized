package kratia.state

import java.util.UUID

case class Instance[A](id: UUID, model: A)
