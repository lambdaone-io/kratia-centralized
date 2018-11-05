name := "kratia-centralized"

description := "Centralized version of a Kratia Engine."

scalaVersion := "2.12.7"

scalacOptions ++= Seq(
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Ypartial-unification"
)

lazy val http4s = "0.19.0-M4"

lazy val circe = "0.10.0"

libraryDependencies ++= List(
  "com.typesafe" % "config" % "1.3.2",
  "io.circe" %% "circe-generic" % circe,
  "io.circe" %% "circe-parser" % circe,
  "org.http4s" %% "http4s-blaze-server" % http4s,
  "org.http4s" %% "http4s-circe" % http4s,
  "org.http4s" %% "http4s-dsl" % http4s,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)