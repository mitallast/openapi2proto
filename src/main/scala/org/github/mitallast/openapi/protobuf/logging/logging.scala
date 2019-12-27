package org.github.mitallast.openapi.protobuf.logging

import io.circe.{Encoder, Json}
import org.yaml.snakeyaml.error.Mark

object implicits {

  implicit val LogMessageEncoder: Encoder[LogMessage] = Encoder.encodeJson.contramap { log =>
    val level = log match {
      case _: InfoMessage    => "INFO"
      case _: WarningMessage => "WARNING"
      case _: ErrorMessage   => "ERROR"
    }
    val mark = log.mark
    Json.obj(
      ("file", Json.fromString(mark.getName)),
      ("line", Json.fromInt(mark.getLine + 1)),
      ("column", Json.fromInt(mark.getColumn + 1)),
      ("level", Json.fromString(level)),
      ("message", Json.fromString(log.message)),
      ("snippet", Json.fromString(log.mark.get_snippet()))
    )
  }
}

sealed trait LogMessage {
  def message: String
  def mark: Mark
  def line: String = s"${mark.getName}:${mark.getLine}:${mark.getColumn}"
  def messageWithLine: String = s"[$line] $message"
}
final case class InfoMessage(mark: Mark, message: String) extends LogMessage {
  override def toString: String = s"[INFO   ][$line] $message"
}
final case class WarningMessage(mark: Mark, message: String) extends LogMessage {
  override def toString: String = s"[WARNING][$line] $message"
}
final case class ErrorMessage(mark: Mark, message: String) extends LogMessage {
  override def toString: String = s"[ERROR  ][$line] $message"
}
