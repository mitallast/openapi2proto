package org.github.mitallast.openapi.protobuf.resolver

import java.io.{File, FileReader}

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
  def resolve(filepath: Scalar[String]): Result[OpenAPI]
}

object OpenAPIResolver {
  def apply(files: Map[String, OpenAPI]): OpenAPIResolver = new FixedResolver(files)

  def apply(): OpenAPIResolver = new FileResolver()

  final class FixedResolver(files: Map[String, OpenAPI]) extends OpenAPIResolver {

    def resolved(): Result[Vector[OpenAPI]] = pure(files.values.toVector)

    def resolve(filepath: Scalar[String]): Result[OpenAPI] =
      for {
        _ <- require(files.contains(filepath.value), filepath.node, "file not resolved")
        api <- pure(files(filepath.value))
      } yield api
  }

  final class FileResolver extends OpenAPIResolver {
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
}
