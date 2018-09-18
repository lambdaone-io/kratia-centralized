package kratia.utils

import java.util.UUID

case class Instance[A](id: UUID, model: A)
