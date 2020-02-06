package org.github.mitallast.openapi.protobuf.resolver

import java.io.{File, FileReader}
import java.nio.file.Path

import cats.data._
import cats.effect.{ExitCode, IO}
import cats.implicits._
import org.yaml.snakeyaml.error.Mark
import org.yaml.snakeyaml.nodes._
import org.github.mitallast.openapi.protobuf.common._
import org.github.mitallast.openapi.protobuf.logging._
import org.github.mitallast.openapi.protobuf.parser.{OpenAPI, OpenAPIParser, Scalar}

import scala.collection.concurrent

trait OpenAPIResolver {
  def resolved(): Result[Vector[OpenAPI]]
  def resolve(currentFile: Path, filepath: Scalar[String]): Result[OpenAPI]
}

object OpenAPIResolver {
  def apply(files: Map[Path, OpenAPI]): OpenAPIResolver = new FixedResolver(files)

  def apply(): OpenAPIResolver = new FileResolver()

  final class FixedResolver(files: Map[Path, OpenAPI]) extends OpenAPIResolver {

    def resolved(): Result[Vector[OpenAPI]] = pure(files.values.toVector)

    def resolve(currentFile: Path, filepath: Scalar[String]): Result[OpenAPI] =
      for {
        path <- delay(Path.of(filepath.value))
        _ <- require(files.contains(path), filepath.node, "file not resolved")
        api <- pure(files(path))
      } yield api
  }

  final class FileResolver extends OpenAPIResolver {
    private val files = concurrent.TrieMap.empty[Path, OpenAPI]

    def resolved(): Result[Vector[OpenAPI]] = pure(files.values.toVector)

    def resolve(currentFile: Path, filepath: Scalar[String]): Result[OpenAPI] =
      for {
        absolutePath <- resolvePath(currentFile, filepath)
        exists <- delay(files.contains(absolutePath))
        api <- if (exists) pure(files(absolutePath)) else parse(filepath, absolutePath)
      } yield api

    private def resolvePath(currentFile: Path, filepath: Scalar[String]): Result[Path] =
      for {
        path <- delay(Path.of(filepath.value))
        _ <- require(!path.isAbsolute, filepath.node, "compiler error, must be relative path")
      } yield currentFile.getParent.resolve(path).normalize()

    private def parse(path: Scalar[String], filepath: Path): Result[OpenAPI] =
      for {
        file <- delay(filepath.toFile)
        _ <- require(file.exists(), path.node, "file not found")
        reader <- delay(new FileReader(file))
        api <- OpenAPIParser.parse(reader, filepath)
        _ = files.put(filepath, api)
      } yield api
  }
}
