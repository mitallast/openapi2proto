package org.github.mitallast.openapi.protobuf

import cats.data.{EitherT, WriterT}
import cats.effect.{ExitCode, IO}
import cats.implicits._
import org.github.mitallast.openapi.protobuf.logging._
import org.github.mitallast.openapi.protobuf.parser.ScalarMap
import org.github.mitallast.openapi.protobuf.resolver.OpenAPIResolver
import org.yaml.snakeyaml.error.Mark
import org.yaml.snakeyaml.nodes.Node

package object common {
  type Extensions = ScalarMap[String, Node]
  type Logging[A] = WriterT[IO, Vector[LogMessage], A]
  type Result[A] = EitherT[Logging, ExitCode, A]
  type Resolver = OpenAPIResolver

  @inline def log(message: LogMessage): Result[Unit] = EitherT.liftF(WriterT.tell(Vector(message)))

  @inline def info(mark: Mark, message: String): Result[Unit] = log(InfoMessage(mark, message))
  @inline def info(node: Node, message: String): Result[Unit] = info(node.getStartMark, message)

  @inline def warning(mark: Mark, message: String): Result[Unit] = log(WarningMessage(mark, message))
  @inline def warning(node: Node, message: String): Result[Unit] = warning(node.getStartMark, message)

  @inline def error[A](err: ErrorMessage): Result[A] = log(err).flatMap(_ => EitherT.leftT(ExitCode.Error))
  @inline def error[A](mark: Mark, message: String): Result[A] = error(ErrorMessage(mark, message))
  @inline def error[A](node: Node, message: String): Result[A] = error(node.getStartMark, message)

  @inline def pure[A](value: A): Result[A] = EitherT.pure(value)
  @inline def delay[A](body: => A): Result[A] = EitherT.liftF(WriterT.liftF(IO.delay(body)))
  @inline def left: Result[Unit] = EitherT.leftT(ExitCode.Error)
  val unit: Result[Unit] = pure(())

  @inline
  def require(test: Boolean, node: Node, message: => String): Result[Unit] = if (test) unit else error(node, message)
}
