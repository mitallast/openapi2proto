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

    val http4sVersion = "0.20.15"
    val http4sDsl = "org.http4s" %% "http4s-dsl" % http4sVersion
    val http4sServer = "org.http4s" %% "http4s-blaze-server" % http4sVersion
    val http4sCirce = "org.http4s" %% "http4s-circe" % http4sVersion

    val circeGeneric = "io.circe" %% "circe-generic" % "0.11.2"

    val swaggerParser = "io.swagger.parser.v3" % "swagger-parser" % "2.0.16" excludeAll (
      ExclusionRule(organization = "io.swagger.parser.v3", name = "swagger-parser-v2-converter"),
      ExclusionRule(organization = "org.slf4j"),
    )
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
    http4sDsl,
    http4sServer,
    http4sCirce,
    circeGeneric,
    swaggerParser,
    scalacheck,
    scalatest
  )
}
