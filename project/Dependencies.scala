import sbt._, Keys._

object Dependencies {
  val resolvers = Seq(
    "Typesafe Releases" at "https://repo.typesafe.com/typesafe/maven-releases/",
    "Apache Staging" at "https://repository.apache.org/content/groups/staging/",
    Resolver.jcenterRepo,
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("staging"),
    Resolver.bintrayRepo("hseeberger", "maven"),
    Resolver.bintrayRepo("scalameta", "maven"),
    Resolver.bintrayRepo("akka", "maven"),
    Resolver.bintrayRepo("slamdata-inc", "maven-public")
  )

  object Compile {
    val slf4jApi = "org.slf4j" % "slf4j-api" % "1.7.29"
    val slf4jSimple = "org.slf4j" % "slf4j-simple" % "1.7.29"
    val log4s = "org.log4s" %% "log4s" % "1.8.2"

    val scopt = "com.github.scopt" %% "scopt" % "4.0.0-RC2"

    val kittens = "org.typelevel" %% "kittens" % "2.0.0"

    val http4sVersion = "0.21.0-M6"
    val http4sDsl = "org.http4s" %% "http4s-dsl" % http4sVersion
    val http4sServer = "org.http4s" %% "http4s-blaze-server" % http4sVersion
    val http4sCirce = "org.http4s" %% "http4s-circe" % http4sVersion

    val circeGeneric = "io.circe" %% "circe-generic" % "0.12.3"

    val snakeyaml = "org.yaml" % "snakeyaml" % "1.25"

    // required only for compilation to GraalVM native-image
    val substratevm = "com.oracle.substratevm" % "svm" % "19.1.0" % Provided
  }

  object Test {
    val scalacheck = "org.scalacheck" %% "scalacheck" % "1.14.2" % "test"
    val scalatest = "org.scalatest" %% "scalatest" % "3.0.8" % "test"
  }

  import Compile._
  import Test._

  val common = Seq(
    slf4jApi,
    slf4jSimple,
    log4s,
    scopt,
    kittens,
    http4sDsl,
    http4sServer,
    http4sCirce,
    circeGeneric,
    snakeyaml,
    substratevm,
    scalacheck,
    scalatest
  )
}
