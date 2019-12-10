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

  object V {
    val log4j = "2.12.1"
  }

  object Compile {
    val log4jApi = "org.apache.logging.log4j" % "log4j-api" % V.log4j
    val log4jApiScala = "org.apache.logging.log4j" %% "log4j-api-scala" % "11.0"
    val log4jCore = "org.apache.logging.log4j" % "log4j-core" % V.log4j % "runtime"
    val log4jSlf4j = "org.apache.logging.log4j" % "log4j-slf4j-impl" % V.log4j
    val log4jJcl = "org.apache.logging.log4j" % "log4j-jcl" % V.log4j

    val swaggerParser = "io.swagger.parser.v3" % "swagger-parser" % "2.0.16"

    val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"

    val enumeratum = "com.beachape" %% "enumeratum" % "1.5.13"

    val commonsText = "org.apache.commons" % "commons-text" % "1.8"
  }

  object Test {
    val scalacheck = "org.scalacheck" %% "scalacheck" % "1.14.2" % "test"
    val scalatest = "org.scalatest" %% "scalatest" % "3.0.8" % "test"
  }

  import Compile._
  import Test._

  val common = Seq(
    log4jApi,
    log4jApiScala,
    log4jCore,
    log4jSlf4j,
    log4jJcl,
    swaggerParser,
    shapeless,
    enumeratum,
    commonsText,
    scalacheck,
    scalatest
  )
}
