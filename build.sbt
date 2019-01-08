name := "kratia-centralized"

description := "Centralized version of a KratiaService Engine."

scalaVersion := "2.12.7"

scalacOptions ++= Seq(
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Ypartial-unification"
)

lazy val http4s = "0.20.0-M3"

lazy val circe = "0.10.1"

libraryDependencies ++= List(
  "com.typesafe" % "config" % "1.3.2",
  "com.pauldijou" %% "jwt-core" % "1.0.0",
  "com.pauldijou" %% "jwt-circe" % "1.0.0",
  "io.circe" %% "circe-generic" % circe,
  "io.circe" %% "circe-parser" % circe,
  "org.http4s" %% "http4s-blaze-server" % http4s,
  "org.http4s" %% "http4s-blaze-client" % http4s,
  "org.http4s" %% "http4s-circe" % http4s,
  "org.http4s" %% "http4s-dsl" % http4s,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.typelevel" %% "discipline" % "0.10.0" % "test"
)

libraryDependencies ++= Seq(

  // Start with this one
  "org.tpolecat" %% "doobie-core"      % "0.6.0",

  // And add any of these as needed
  "org.tpolecat" %% "doobie-h2"        % "0.6.0",          // H2 driver 1.4.197 + type mappings.
  "org.tpolecat" %% "doobie-hikari"    % "0.6.0",          // HikariCP transactor.
  "org.tpolecat" %% "doobie-postgres"  % "0.6.0",          // Postgres driver 42.2.5 + type mappings.
  "org.tpolecat" %% "doobie-specs2"    % "0.6.0" % "test", // Specs2 support for typechecking statements.
  "org.tpolecat" %% "doobie-scalatest" % "0.6.0" % "test"  // ScalaTest support for typechecking statements.

)


resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)