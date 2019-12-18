package org.github.mitallast.openapi.protobuf.compiler

import java.util.Collections

import io.swagger.v3.oas.models.media.ObjectSchema

import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

private[compiler] object util {
  val schemaRef: Regex = "^#/components/schemas/([a-zA-Z0-9_]+)$".r
  val constantRegex: Regex = "^[a-zA-Z0-9]+(_([a-zA-Z0-9]+))*$".r
  val camelCaseRegex: Regex = "^([a-zA-Z][a-z]+)([A-Z][a-z]+)*$".r
  val extractIdentifierRegex: Regex = "^([a-zA-Z0-9_]+\\.)*([a-zA-Z0-9_]+)$".r

  def cleanup(string: String): String = string.trim.replace('-', '_').replaceAll("[^a-zA-Z0-9_]", "")

  def camelCaseToUnderscore(string: String): String =
    "([a-z])([A-Z])".r.replaceAllIn(string, m => m.group(1) + "_" + m.group(2)).toLowerCase

  def underscoreToCamelCase(string: String): String =
    "_([a-zA-Z])".r.replaceAllIn(string, _.group(1).toUpperCase())

  def extractIdentifier(string: String): String = string match {
    case extractIdentifierRegex(packageName, identifier) => identifier
  }

  def extension[T](ext: java.util.Map[String, Object], key: String): Option[T] =
    Option(ext)
      .getOrElse(Collections.emptyMap())
      .asScala
      .toMap
      .get(key)
      .map(s => s.asInstanceOf[T])

  def reserved(schema: ObjectSchema): Seq[Int] =
    extension[java.util.List[Int]](schema.getExtensions, "x-proto-reserved")
      .getOrElse(Collections.emptyList())
      .asScala
      .toSeq
}
