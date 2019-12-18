package org.github.mitallast.openapi.protobuf

import java.io.File
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import io.swagger.v3.parser.OpenAPIV3Parser
import org.github.mitallast.openapi.protobuf.compiler.{Error, Info, LogMessage, ProtoCompiler, Warning}
import org.github.mitallast.openapi.protobuf.writer.Writer
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.http4s.server.Router
import org.log4s._
import scopt.{OParser, Read}

import scala.concurrent.ExecutionContext

sealed trait Command
case object Help extends Command
case object Compile extends Command
case object Server extends Command

final case class Config(
  command: Command = Help,
  filepath: Path = Paths.get("petstore.yaml"),
  port: Int = 8081,
  host: String = "localhost"
)

final case class CompileResponse(log: Vector[LogMessage], source: Option[String])

object Main extends IOApp {
  System.setProperty("org.slf4j.simpleLogger.showShortLogName", "true")

  private implicit val pathRead: Read[Path] = Read.reads { Paths.get(_) }

  private val builder = OParser.builder[Config]

  private val parser = {
    import builder._
    OParser.sequence(
      programName("openapi2proto"),
      head("openapi2proto", "v0.1-alpha3"),
      cmd("compile")
        .action((_, c) => c.copy(command = Compile))
        .text("compile openapi v3 to protobuf v3 with gRPC")
        .children(
          arg[Path]("<file>")
            .required()
            .action((path, c) => c.copy(filepath = path))
            .text("openapi file in yaml format")
        ),
      cmd("server")
        .action((_, c) => c.copy(command = Server))
        .text("run web server with ui and rest api")
        .children(
          opt[Int]("port")
            .abbr("p")
            .action((port, c) => c.copy(port = port))
            .text("port listen"),
          opt[String]("host")
            .abbr("h")
            .action((host, c) => c.copy(host = host))
            .text("host listen")
        )
    )
  }

  def run(args: List[String]): IO[ExitCode] =
    OParser.parse(parser, args, Config()) match {
      case Some(config) =>
        config.command match {
          case Help    => help()
          case Compile => compile(config.filepath)
          case Server  => server(config.port, config.host)
        }
      case _ => help()
    }

  private def help(): IO[ExitCode] = IO {
    Console.err.println(OParser.usage(parser))
    ExitCode.Error
  }

  private def compile(filepath: Path): IO[ExitCode] = IO {
    val logger = getLogger("compiler")
    val api = new OpenAPIV3Parser().read(filepath.toString)
    val (logging, result) = ProtoCompiler.compile(api, filepath.toString)
    logging.foreach {
      case Info(message)    => logger.info(message)
      case Warning(message) => logger.warn(message)
      case Error(message)   => logger.error(message)
    }
    result match {
      case Left(exitCode) =>
        val errors = logging.count {
          case _: Error => true
          case _        => false
        }
        logger.info(s"errors: $errors")
        exitCode
      case Right(protoFile) =>
        val source = Writer.writeFile(protoFile)
        println(source)
        Files.write(
          Paths.get(protoFile.path.value),
          source.getBytes("UTF-8"),
          StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING
        )
        ExitCode.Success
    }
  }

  private def server(port: Int, host: String): IO[ExitCode] = {
    val logger = getLogger("server")
    val api = HttpRoutes.of[IO] {
      case request @ POST -> Root / "compile" =>
        request.decode[String] { yaml =>
          for {
            source <- IO {
              val api = new OpenAPIV3Parser().readContents(yaml).getOpenAPI
              ProtoCompiler.compile(api, "source.proto") match {
                case (logs, Left(exitCode)) =>
                  CompileResponse(logs, None)
                case (logs, Right(protoFile)) =>
                  val source = Writer.writeFile(protoFile)
                  CompileResponse(logs, Some(source))
              }
            }
            response <- Ok(source.asJson)
          } yield response
        }
    }

    val staticFiles = HttpRoutes.of[IO] {
      case req @ GET -> Root =>
        logger.info("requested /")
        StaticFile
          .fromFile(new File("src/main/resources/index.html"), ExecutionContext.global, Some(req))
          .orElse(StaticFile.fromResource("/index.html", ExecutionContext.global, Some(req)))
          .getOrElseF(NotFound())
      case req @ GET -> "static" /: path =>
        logger.info(s"requested $path")
        StaticFile
          .fromResource(path.toString, ExecutionContext.global, Some(req))
          .getOrElseF(NotFound())
    }

    val httpApp = Router("/api" -> api, "/" -> staticFiles).orNotFound

    BlazeServerBuilder[IO]
      .bindHttp(port, host)
      .withHttpApp(httpApp)
      .resource
      .use(_ => IO.never)
      .as(ExitCode.Success)
  }
}
