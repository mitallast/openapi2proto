package org.github.mitallast.openapi.protobuf.parser

import scala.util.matching.Regex

private[parser] object util {
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
    case extractIdentifierRegex(_, identifier) => identifier
  }
}
