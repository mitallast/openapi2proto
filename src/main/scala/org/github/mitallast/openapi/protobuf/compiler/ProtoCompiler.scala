package org.github.mitallast.openapi.protobuf.compiler

import java.nio.file.Paths
import java.util.Collections

import io.swagger.v3.oas.models.{OpenAPI, Operation, PathItem}
import io.swagger.v3.oas.models.media.{
  ArraySchema,
  BooleanSchema,
  Content,
  DateSchema,
  DateTimeSchema,
  IntegerSchema,
  NumberSchema,
  ObjectSchema,
  Schema,
  StringSchema
}
import org.github.mitallast.openapi.protobuf.model._
import org.log4s._

import scala.collection.JavaConverters._
import scala.util.matching.Regex

private object util {
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

  private val constantRegex = "^[a-zA-Z0-9]+(_([a-zA-Z0-9]+))*$".r
  private val camelCaseRegex = "^([a-zA-Z][a-z]+)([A-Z][a-z]+)*$".r

  def normalizeEnumValue(string: String): Identifier = {
    val clean = cleanup(string)
    clean match {
      case constantRegex  => Identifier(clean.toUpperCase)
      case camelCaseRegex => Identifier(camelCaseToUnderscore(clean).toUpperCase())
      case _              => Identifier(camelCaseToUnderscore(clean).toUpperCase())
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

object ProtoCompiler {
  private val logger = getLogger

  import util._

  def compile(api: OpenAPI, path: String): ProtoFile = {
    val protoFile = ProtoFile.builder(packageName(api, path))
    protoFile += ImportStatement("google/api/annotations.proto")
    protoFile += ImportStatement("google/protobuf/wrappers.proto")

    compileComponents(protoFile, api)
    compileService(protoFile, api)
    protoFile.build
  }

  private def compileComponents(protoFile: ProtoFileBuilder, api: OpenAPI): Unit =
    if (api.getComponents != null && api.getComponents.getSchemas != null) {
      api.getComponents.getSchemas.asScala.foreach {
        case (typeName, schema: ObjectSchema) =>
          compileComponentObject(protoFile, typeName, schema)
        case (typeName, schema: StringSchema) if schema.getEnum != null =>
          compileComponentEnum(protoFile, typeName, schema)
        case (typeName, schema) =>
          logger.warn(s"ignore component: $typeName schema=${schema.getType}")
      }
    }

  private def compileComponentEnum(protoFile: ProtoFileBuilder, typeName: String, schema: StringSchema): Unit = {
    logger.info(s"enum=$typeName schema=${schema.getType}")
    require(schema.getEnum != null, "enum is required in schema")
    val enumBuilder = Enum.builder(normalizeType(typeName))
    for (elem <- schema.getEnum.asScala) {
      enumBuilder += EnumValue(normalizeEnumValue(elem), enumBuilder.nextValueNum, Vector.empty)
    }
    protoFile += enumBuilder.build
  }

  private def compileComponentObject(protoFile: ProtoFileBuilder, typeName: String, schema: ObjectSchema): Unit = {
    logger.info(s"message=$typeName schema=${schema.getType}")
    val messageBuilder = Message.builder(Identifier(typeName))
    messageBuilder.reserved(reserved(schema))
    require(schema.get$ref() == null, "component message ref is not supported")
    val requiredFields = Option(schema.getRequired).map(_.asScala.toSet).getOrElse(Set.empty)
    if (schema.getProperties != null && !schema.getProperties.isEmpty) {
      schema.getProperties.asScala.foreach {
        case (fieldNameRaw, schema) =>
          logger.info(s"field=$fieldNameRaw schema=${schema.getType}")
          val required = requiredFields.contains(fieldNameRaw)
          compileField(messageBuilder, fieldNameRaw, schema, schema.getExtensions, required)
      }
    }
    protoFile += messageBuilder.build
  }

  private def compileService(protoFile: ProtoFileBuilder, api: OpenAPI): Unit = {
    val service = Service.builder(serviceName(api))
    api.getPaths.asScala.foreach {
      case (path, item) =>
        item.readOperationsMap.asScala.foreach {
          case (method, op) =>
            compileRpc(protoFile, service, method, path, op)
        }
    }
    protoFile += service.build
  }

  private def compileRpc(
    protoFile: ProtoFileBuilder,
    service: ServiceBuilder,
    method: PathItem.HttpMethod,
    path: String,
    op: Operation
  ): Unit = {
    val requestType = normalizeType(op.getOperationId.capitalize + "Request")
    val responseType = normalizeType(op.getOperationId.capitalize + "Response")

    val http = RpcOption.builder(FullIdentifier("google.api.http"))
    http += OptionStatement(Identifier(method.toString.toLowerCase), path)

    val rpc = RpcStatement.builder(Identifier(op.getOperationId))
    rpc.withRequestType(requestType)
    rpc.withResponseType(responseType)

    val requestBuilder = Message.builder(requestType)
    if (op.getParameters != null) {
      op.getParameters.asScala.foreach { p =>
        require(p.getName != null, "parameter name is required")
        require(p.getName.nonEmpty, "parameter name is required")
        require(p.getSchema != null, "parameter schema is required")
        require(p.getSchema.get$ref() == null, "parameter schema $ref is not supported")
        compileField(requestBuilder, p.getName, p.getSchema, p.getExtensions, p.getRequired)
      }
    }
    if (op.getRequestBody != null) {
      require(op.getRequestBody.get$ref() == null, "request body $ref is not supported")
      val content = op.getRequestBody.getContent
      require(content != null, "content is required")
      compileContent(http, requestBuilder, content, Identifier("request_body"), Identifier("body"))
    }
    protoFile += requestBuilder.build

    val responseBuilder = Message.builder(responseType)
    val responses = op.getResponses
    require(responses != null, "responses is required")
    val response = if (responses.containsKey("default")) {
      val response = responses.getDefault
      Some(response)
    } else if (responses.containsKey("200")) {
      val response = responses.get("200")
      Some(response)
    } else if (responses.containsKey("201")) {
      val response = responses.get("201")
      Some(response)
    } else {
      None
    }
    response.foreach { response =>
      require(response.get$ref() == null, "response $ref is not supported")
      val content = response.getContent
      if (content != null) {
        compileContent(http, responseBuilder, content, Identifier("response_body"), Identifier("response_body"))
      }
    }
    protoFile += responseBuilder.build

    rpc += http.build
    service += rpc.build
  }

  private def compileContent(
    options: RpcOptionBuilder,
    builder: MessageBuilder,
    content: Content,
    defaultFieldName: Identifier,
    context: Identifier
  ): Unit = {
    require(!content.isEmpty, s"$context content should not be empty")
    require(content.size() == 1, s"$context content should contain only one media type")
    val (mediaType, media) = content.asScala.head
    require(media != null, "media type is required")
    val schema = media.getSchema
    require(schema != null, "media type schema is required")

    val fieldName = extension[String](media.getExtensions, "x-proto-field")
      .map(normalizeFieldName)
      .getOrElse(defaultFieldName)
    val fieldNum = extension[Int](media.getExtensions, "x-proto-field-id")
      .getOrElse(builder.nextFieldNum)

    mediaType match {
      case "application/json" =>
        compileField(builder, fieldName, fieldNum, schema, required = true)
      case "text/plain" =>
        require(schema.getType == "string", s"$fieldName: schema type should be string")
        builder += NormalField(Identifier("string"), fieldName, fieldNum, Vector.empty)
      case "application/octet-stream" =>
        require(schema.getType == "string", s"$fieldName: schema type should be string")
        require(schema.getFormat == "binary", s"$fieldName: string format should be binary")
        builder += NormalField(Identifier("bytes"), fieldName, fieldNum, Vector.empty)
    }
    options += OptionStatement(context, fieldName.value)
  }

  private def compileField(
    builder: MessageBuilder,
    fiendName: String,
    schema: Schema[_],
    extensions: java.util.Map[String, Object],
    required: Boolean
  ): Unit = {
    val fieldIdentifier = extension[String](extensions, "x-proto-field")
      .map(normalizeFieldName)
      .getOrElse(normalizeFieldName(fiendName))
    val fieldNum = extension[Int](extensions, "x-proto-field-id")
      .getOrElse(builder.nextFieldNum)
    compileField(builder, fieldIdentifier, fieldNum, schema, required)
  }

  private def compileField(
    builder: MessageBuilder,
    fieldIdentifier: Identifier,
    fieldNum: Int,
    schema: Schema[_],
    required: Boolean
  ): Unit =
    schema match {
      case integer: IntegerSchema =>
        val typeName = extractInteger(integer, required)
        builder += NormalField(typeName, fieldIdentifier, fieldNum, Vector.empty)
      case number: NumberSchema =>
        val typeName = extractNumber(number, required)
        builder += NormalField(typeName, fieldIdentifier, fieldNum, Vector.empty)
      case string: StringSchema =>
        val typeName = extractString(string, required)
        builder += NormalField(typeName, fieldIdentifier, fieldNum, Vector.empty)
      case date: DateSchema =>
        val typeName = extractDate(date, required)
        builder += NormalField(typeName, fieldIdentifier, fieldNum, Vector.empty)
      case date: DateTimeSchema =>
        val typeName = extractDateTime(date, required)
        builder += NormalField(typeName, fieldIdentifier, fieldNum, Vector.empty)
      case boolean: BooleanSchema =>
        val typeName = extractBoolean(boolean, required)
        builder += NormalField(typeName, fieldIdentifier, fieldNum, Vector.empty)
      case array: ArraySchema =>
        val typeName = extractArrayType(array)
        builder += RepeatedField(typeName, fieldIdentifier, fieldNum, Vector.empty)
      case _ =>
        val typeName = extractComponentRef(schema)
        builder += NormalField(typeName, fieldIdentifier, fieldNum, Vector.empty)
    }

  private def extractInteger(schema: IntegerSchema, required: Boolean): TypeIdentifier = {
    require(schema.get$ref() == null, "$ref is not allowed")
    require(schema.getEnum == null, "enum is not allowed")
    (required, Option(schema.getFormat)) match {
      case (true, Some("int32"))  => Identifier("int32")
      case (true, Some("int64"))  => Identifier("int64")
      case (true, None)           => Identifier("int64")
      case (false, Some("int32")) => FullIdentifier("google.protobuf.Int32Value")
      case (false, Some("int64")) => FullIdentifier("google.protobuf.Int64Value")
      case (false, None)          => FullIdentifier("google.protobuf.Int64Value")
      case (_, Some(format)) =>
        throw new IllegalArgumentException(s"unexpected integer format: $format")
    }
  }

  private def extractNumber(schema: NumberSchema, required: Boolean): TypeIdentifier = {
    require(schema.get$ref() == null, "$ref is not allowed")
    require(schema.getEnum == null, "enum is not allowed")
    (required, Option(schema.getFormat)) match {
      case (true, Some("float"))   => Identifier("float")
      case (true, Some("double"))  => Identifier("double")
      case (true, None)            => Identifier("double")
      case (false, Some("float"))  => FullIdentifier("google.protobuf.FloatValue")
      case (false, Some("double")) => FullIdentifier("google.protobuf.DoubleValue")
      case (false, None)           => FullIdentifier("google.protobuf.DoubleValue")
      case (_, Some(format)) =>
        throw new IllegalArgumentException(s"unexpected number format: $format")
    }
  }

  private def extractString(schema: StringSchema, required: Boolean): TypeIdentifier = {
    require(schema.get$ref() == null, "$ref is not allowed")
    require(schema.getFormat == null, "format is not allowed")
    require(schema.getEnum == null, "inline enum is not supported, use $ref to component")
    if (required) Identifier("string") else FullIdentifier("google.protobuf.StringValue")
  }

  private def extractDate(schema: DateSchema, required: Boolean): TypeIdentifier = {
    require(schema.get$ref() == null, "$ref is not allowed")
    require(schema.getEnum == null, "enum is not allowed")
    if (required) Identifier("string") else FullIdentifier("google.protobuf.StringValue")
  }

  private def extractDateTime(schema: DateTimeSchema, required: Boolean): TypeIdentifier = {
    require(schema.get$ref() == null, "$ref is not allowed")
    require(schema.getEnum == null, "enum is not allowed")
    if (required) Identifier("string") else FullIdentifier("google.protobuf.StringValue")
  }

  private def extractBoolean(schema: BooleanSchema, required: Boolean): TypeIdentifier = {
    require(schema.get$ref() == null, "$ref is not allowed")
    require(schema.getEnum == null, "enum is not allowed")
    if (required) Identifier("bool") else FullIdentifier("google.protobuf.BoolValue")
  }

  private def extractComponentRef(schema: Schema[_]): TypeIdentifier = {
    require(schema.get$ref() != null, "schema $ref is required")
    require(schema.getEnum == null, "enum is not allowed")
    componentRefType(schema.get$ref())
  }

  private def extractArrayType(schema: ArraySchema): TypeIdentifier = {
    require(schema.getItems != null, "array schema is required")
    require(schema.getEnum == null, "enum is not allowed")
    val items = schema.getItems
    items match {
      case integer: IntegerSchema => extractInteger(integer, required = true)
      case number: NumberSchema   => extractNumber(number, required = true)
      case string: StringSchema   => extractString(string, required = true)
      case date: DateSchema       => extractDate(date, required = true)
      case date: DateTimeSchema   => extractDateTime(date, required = true)
      case boolean: BooleanSchema => extractBoolean(boolean, required = true)
      case _                      => extractComponentRef(items)
    }
  }
}
