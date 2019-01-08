package lambdaone.github.events

import lambdaone.github.models.PullRequest

/**
  * https://developer.github.com/v3/activity/events/types/#pullrequestevent
  */
case class PullRequestEvent(
  action: String,
  number: Int,
  pull_request: PullRequest,
  installation: InstallationLight
)
