package lambdaone.github.models

case class PullRequest (
  url: String,
  id: Int,
  html_url: String,
  state: String,
  user: GithubUser,
  body: String,
  created_at: String,
  head: Commit
)
