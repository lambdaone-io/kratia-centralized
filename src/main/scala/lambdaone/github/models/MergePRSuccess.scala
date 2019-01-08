package lambdaone.github.models

case class MergePRSuccess(
  sha: String,
  merged: Boolean,
  message: String
)
