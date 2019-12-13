package org.github.mitallast.openapi.protobuf.model

object lexical {

  object letters {

    import identifiers._

    val letter: String = "[a-zA-Z]"
    val decimalDigit: String = "[0-9]"
    val octalDigit: String = "[0-7]"
    val hexDigit: String = "[0-9a-fA-F]"

    val decimalLit = s"([1-9]$decimalDigit+)"
    val octalLit = s"(0$octalDigit*)"
    val hexLit = s"(0[xX]$hexDigit+)"
    val intLit = s"($decimalLit|$octalLit|$hexLit)"

    val decimals = s"($decimalDigit+)"
    val exponent = s"([eE][-+]$decimals)"
    val floatLit = s"(inf|nan|$decimals\\.$decimals?$exponent?|$decimals$exponent)"

    val boolLit = s"(true|false)"

    val charEscape = "(\\[abfnrtv\\\'\"])"
    val octEscape = s"(\\$octalDigit$octalDigit$octalDigit)"
    val hexEscape = s"(\\[xX]$hexDigit$hexDigit)"
    val charVal: String = s"""($hexEscape|$octEscape|$charEscape|[^\\n\\r\\\\\\\0])"""
    val strLit: String = s"""('($charVal)+'|"($charVal)+")"""
    val constant = s"($fullIdent|[-+]$intLit|[-+]$floatLit|$strLit|$boolLit)"
  }

  object identifiers {

    import letters._

    val ident = s"($letter($letter|$decimalDigit|_)*)"
    val fullIdent = s"($ident(\\.$ident)*)"
    val messageName: String = ident
    val enumName: String = ident
    val fieldName: String = ident
    val oneOfName: String = ident
    val mapName: String = ident
    val serviceName: String = ident
    val rpcName: String = ident
    val messageType: String = s"\\.?($ident\\.)*$messageName"
    val enumType: String = s"\\.?($ident\\.)*$enumName"

    val protoPath: String = s"($ident\\/)*$ident\\.proto"
  }

  @inline
  def valid(value: String, pattern: String): Unit =
    require(value.matches(s"^$pattern$$"), s"$value does not match $pattern")

  @inline
  def validate(value: String, pattern: String): Boolean =
    value.matches(s"^$pattern$$")
}
