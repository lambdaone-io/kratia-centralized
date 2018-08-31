package kratia

import java.time.Instant

trait Reactive[F[_], A] {

  def timeStream(a: A, time: Instant): F[Unit]
}
