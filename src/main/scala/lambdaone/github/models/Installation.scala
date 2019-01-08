package lambdaone.github.models

case class Installation(
  id: Int,
  access_tokens_url: String,
  account: GithubAccount
)

