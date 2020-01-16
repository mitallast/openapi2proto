package org.github.mitallast.openapi.protobuf.resolver

import java.io.{File, FileReader}

import cats.data._
import cats.effect.{ExitCode, IO}
import cats.implicits._
import org.yaml.snakeyaml.error.Mark
import org.yaml.snakeyaml.nodes._
import org.github.mitallast.openapi.protobuf.logging._
import org.github.mitallast.openapi.protobuf.parser.{OpenAPI, OpenAPIParser, Scalar}

import scala.collection.concurrent

trait OpenAPIResolver[F[_]] {
  def resolved(): F[Vector[OpenAPI]]
  def resolve(filepath: Scalar[String]): F[OpenAPI]
}

object OpenAPIResolver {
  type Logging[A] = WriterT[IO, Vector[LogMessage], A]
  type Result[A] = EitherT[Logging, ExitCode, A]

  @inline def log(message: LogMessage): Result[Unit] = EitherT.liftF(WriterT.tell(Vector(message)))
  @inline def info(node: Node, message: String): Result[Unit] = log(InfoMessage(node.getStartMark, message))
  @inline def warning(mark: Mark, message: String): Result[Unit] = log(WarningMessage(mark, message))
  @inline def warning(node: Node, message: String): Result[Unit] = warning(node.getStartMark, message)
  @inline def error[A](err: ErrorMessage): Result[A] = log(err).flatMap(_ => EitherT.leftT(ExitCode.Error))
  @inline def error[A](mark: Mark, message: String): Result[A] = error(ErrorMessage(mark, message))
  @inline def error[A](node: Node, message: String): Result[A] = error(node.getStartMark, message)
  @inline def pure[A](value: A): Result[A] = EitherT.pure(value)
  @inline def delay[A](body: => A): Result[A] = EitherT.liftF(WriterT.liftF(IO.delay(body)))
  @inline val unit: Result[Unit] = pure(())

  def apply(files: Map[String, OpenAPI]): OpenAPIResolver[Result] = new FixedResolver(files)

  def apply(): OpenAPIResolver[Result] = new FileResolver()

  final class FixedResolver(files: Map[String, OpenAPI]) extends OpenAPIResolver[Result] {

    def resolved(): Result[Vector[OpenAPI]] = pure(files.values.toVector)

    def resolve(filepath: Scalar[String]): Result[OpenAPI] =
      for {
        _ <- require(files.contains(filepath.value), filepath.node, "file not resolved")
        api <- pure(files(filepath.value))
      } yield api
  }

  final class FileResolver extends OpenAPIResolver[Result] {
    private val files = concurrent.TrieMap.empty[String, OpenAPI]

    def resolved(): Result[Vector[OpenAPI]] = pure(files.values.toVector)

    def resolve(filepath: Scalar[String]): Result[OpenAPI] =
      for {
        exists <- delay(files.contains(filepath.value))
        api <- if (exists) pure(files(filepath.value)) else parse(filepath)
      } yield api

    def parse(filepath: Scalar[String]): Result[OpenAPI] =
      for {
        file <- delay(new File(filepath.value))
        _ <- require(file.exists(), filepath.node, "file not found")
        reader <- delay(new FileReader(file))
        api <- OpenAPIParser.parse(reader, filepath.value)
        _ = files.put(filepath.value, api)
      } yield api
  }

  def require(f: Boolean, node: Node, message: => String): Result[Unit] =
    if (f) unit else error(node, message)
}
