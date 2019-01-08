package lambdaone.github.events

import lambdaone.github.models.Installation

case class InstallationEvent(
  action: String,
  installation: Installation
)
