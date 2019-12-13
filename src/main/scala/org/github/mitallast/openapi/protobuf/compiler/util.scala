package org.github.mitallast.openapi.protobuf.compiler

import java.nio.file.Paths
import java.util.Collections

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.media.ObjectSchema
import org.github.mitallast.openapi.protobuf.model._

import scala.collection.JavaConverters._
import scala.util.matching.Regex

private[compiler] object util {
  val schemaRef: Regex = "^#/components/schemas/([a-zA-Z0-9_.]+)$".r

  def cleanup(string: String): String = string.trim.replace('-', '_').replaceAll("[^a-zA-Z0-9_]", "")

  def camelCaseToUnderscore(string: String): String =
    "([a-z])([A-Z])".r.replaceAllIn(string, m => m.group(1) + "_" + m.group(2)).toLowerCase

  def underscoreToCamelCase(string: String): String =
    "_([a-zA-Z])".r.replaceAllIn(string, _.group(1).toUpperCase())

  def normalizeType(string: String): Identifier =
    Identifier(underscoreToCamelCase(cleanup(string)).capitalize)

  def normalizeFieldName(string: String): Identifier =
    Identifier(camelCaseToUnderscore(cleanup(string)))

  val constantRegex = "^[a-zA-Z0-9]+(_([a-zA-Z0-9]+))*$".r
  val camelCaseRegex = "^([a-zA-Z][a-z]+)([A-Z][a-z]+)*$".r

  def normalizeEnumValue(string: String): Identifier = {
    val clean = cleanup(string)
    clean match {
      case constantRegex()  => Identifier(clean.toUpperCase)
      case camelCaseRegex() => Identifier(camelCaseToUnderscore(clean).toUpperCase())
      case _                => Identifier(camelCaseToUnderscore(clean).toUpperCase())
    }
  }

  def extension[T](ext: java.util.Map[String, Object], key: String): Option[T] =
    Option(ext)
      .getOrElse(Collections.emptyMap())
      .asScala
      .toMap
      .get(key)
      .map(s => s.asInstanceOf[T])

  def packageName(api: OpenAPI, path: String): FullIdentifier = {
    val name = extension[String](api.getInfo.getExtensions, "x-proto-package").getOrElse {
      Paths.get(path).getFileName.toString.replaceAll("\\.[a-zA-Z]{3,5}$", "")
    }
    FullIdentifier(name)
  }

  def serviceName(api: OpenAPI): Identifier = {
    val name = extension[String](api.getInfo.getExtensions, "x-proto-service").getOrElse {
      api.getInfo.getTitle + "Service"
    }
    normalizeType(name)
  }

  def reserved(schema: ObjectSchema): Seq[Int] =
    extension[java.util.List[Int]](schema.getExtensions, "x-proto-reserved")
      .getOrElse(Collections.emptyList())
      .asScala

  def componentRefType(ref: String): Identifier =
    ref match {
      case schemaRef(name) => normalizeType(name)
    }
}
