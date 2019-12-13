package org.github.mitallast.openapi.protobuf.compiler

import java.nio.file.Paths

import cats.data._
import cats.effect.ExitCode
import cats.implicits._
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
import org.github.mitallast.openapi.protobuf.model.{
  lexical,
  Enum,
  EnumValue,
  FullIdentifier,
  Identifier,
  ImportStatement,
  Message,
  MessageBuilder,
  MessageField,
  NormalField,
  OptionStatement,
  ProtoFile,
  ProtoFileBuilder,
  RepeatedField,
  RpcOption,
  RpcOptionBuilder,
  RpcStatement,
  Service,
  ServiceBuilder,
  TypeIdentifier
}

import scala.collection.JavaConverters._

sealed trait LogMessage
final case class Info(message: String) extends LogMessage {
  override def toString: String = s"[INFO   ] $message"
}
final case class Warning(message: String) extends LogMessage {
  override def toString: String = s"[WARNING] $message"
}
final case class Error(message: String) extends LogMessage {
  override def toString: String = s"[ERROR  ] $message"
}

object ProtoCompiler {
  type Extensions = java.util.Map[String, Object]

  type Logging[A] = Writer[Vector[LogMessage], A]
  type Result[A] = EitherT[Logging, ExitCode, A]

  def log(message: LogMessage): Result[Unit] = EitherT.liftF(Writer.tell(Vector(message)))
  def info(message: String): Result[Unit] = log(Info(message))
  def warning(message: String): Result[Unit] = log(Warning(message))
  def error[A](err: Error): Result[A] = log(err).flatMap(_ => EitherT.leftT(ExitCode.Error))
  def error[A](message: String): Result[A] = error(Error(message))

  def compile(api: OpenAPI, path: String): (Vector[LogMessage], Either[ExitCode, ProtoFile]) =
    compileProtoFile(api, path).value.run

  def compileProtoFile(api: OpenAPI, path: String): Result[ProtoFile] =
    for {
      packageName <- compilePackageName(api, path)
      protoFile = ProtoFile.builder(packageName)
      _ = protoFile += ImportStatement("google/api/annotations.proto")
      _ = protoFile += ImportStatement("google/protobuf/wrappers.proto")
      componentsValid <- compileComponents(protoFile, api)
      serviceValid <- compileService(protoFile, api)
      _ <- require(componentsValid, "has invalid components")
      _ <- require(serviceValid, "has invalid service")
    } yield protoFile.build

  def compilePackageName(api: OpenAPI, path: String): Result[FullIdentifier] =
    util.extension[String](api.getExtensions, "x-proto-package") match {
      case Some(value) => compileFullIdentifier(value)
      case None =>
        for {
          _ <- warning("x-proto-package is not defined")
          _ <- info("try extract package name from path")
          raw = util.cleanup(Paths.get(path).getFileName.toString.replaceAll("\\.[a-zA-Z]{3,5}$", ""))
          id <- compileFullIdentifier(raw)
        } yield id
    }

  def compileFullIdentifier(value: String): Result[FullIdentifier] =
    if (lexical.validate(value, lexical.identifiers.fullIdent)) {
      EitherT.pure(FullIdentifier(value))
    } else error("not valid full identifier")

  def compileTypeName(value: String): Result[Identifier] = {
    val formatted = util.underscoreToCamelCase(util.cleanup(value)).capitalize
    compileIdentifier(formatted)
  }

  def compileIdentifier(value: String): Result[Identifier] =
    if (lexical.validate(value, lexical.identifiers.ident)) {
      EitherT.pure(Identifier(value))
    } else error("not valid identifier")

  def compileComponents(protoFile: ProtoFileBuilder, api: OpenAPI): Result[Boolean] =
    Option(api.getComponents.getSchemas)
      .map(_.asScala)
      .getOrElse(Map.empty)
      .toVector
      .traverse[Result, Either[ExitCode, Unit]] {
        case (typeName, schema: ObjectSchema) =>
          compileComponentObject(protoFile, typeName, schema).attempt
        case (typeName, schema: StringSchema) if schema.getEnum != null =>
          compileComponentEnum(protoFile, typeName, schema).attempt
        case (typeName, _) =>
          warning(s"ignore component: $typeName").attempt
      }
      .map(_.forall(_.isRight))

  def compileComponentObject(protoFile: ProtoFileBuilder, typeName: String, schema: ObjectSchema): Result[Unit] =
    for {
      _ <- info(s"message=$typeName schema=${schema.getType}")
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      messageBuilder = Message.builder(Identifier(typeName))
      _ = messageBuilder.reserved(util.reserved(schema))
      requiredFields = Option(schema.getRequired).map(_.asScala.toSet).getOrElse(Set.empty)
      valid <- Option(schema.getProperties)
        .map(_.asScala)
        .getOrElse(Map.empty)
        .toVector
        .traverse[Result, Either[ExitCode, Unit]] {
          case (fieldName, schema) =>
            val required = requiredFields.contains(fieldName)
            compileField(messageBuilder, fieldName, schema, schema.getExtensions, required).attempt
        }
        .map(_.forall(_.isRight))
      _ <- require(valid, "contains invalid fields")
      _ = protoFile += messageBuilder.build
    } yield ()

  def compileComponentEnum(protoFile: ProtoFileBuilder, typeName: String, schema: StringSchema): Result[Unit] =
    for {
      _ <- requireNotRef(schema)
      _ <- requireNoFormat(schema)
      enumId <- compileTypeName(typeName)
      enumBuilder = Enum.builder(enumId)
      _ <- schema.getEnum.asScala.toVector
        .traverse[Result, Either[ExitCode, Unit]] { value =>
          val clean = util.cleanup(value)
          val formatted = clean match {
            case util.constantRegex()  => clean.toUpperCase()
            case util.camelCaseRegex() => util.camelCaseToUnderscore(clean).toUpperCase()
            case _                     => util.camelCaseToUnderscore(clean).toUpperCase()
          }
          (for {
            id <- compileIdentifier(formatted)
            _ = enumBuilder += EnumValue(id, enumBuilder.nextValueNum, Vector.empty)
          } yield ()).attempt
        }
        .map(_.forall(_.isRight))
      _ = protoFile += enumBuilder.build
    } yield ()

  def compileServiceName(api: OpenAPI): Result[Identifier] =
    util.extension[String](api.getExtensions, "x-proto-service") match {
      case Some(value) => compileIdentifier(value)
      case None =>
        for {
          _ <- warning("x-proto-service is not defined")
          _ <- info("try extract service name from title")
          id <- compileTypeName(api.getInfo.getTitle + "Service")
        } yield id
    }

  def compileService(protoFile: ProtoFileBuilder, api: OpenAPI): Result[Boolean] =
    for {
      _ <- info("compile service")
      serviceName <- compileServiceName(api)
      serviceBuilder = Service.builder(serviceName)
      valid <- api.getPaths.asScala.toVector
        .flatMap {
          case (path, item) =>
            item.readOperationsMap.asScala.map {
              case (method, op) =>
                (path, method, op)
            }
        }
        .traverse[Result, Either[ExitCode, Unit]] {
          case (path, method, op) =>
            compileRpc(protoFile, serviceBuilder, method, path, op).attempt
        }
        .map(_.forall(_.isRight))
      _ = protoFile += serviceBuilder.build
    } yield valid

  def compileRpc(
    protoFile: ProtoFileBuilder,
    service: ServiceBuilder,
    method: PathItem.HttpMethod,
    path: String,
    op: Operation
  ): Result[Unit] =
    for {
      _ <- require(op.getOperationId != null, "require operationId")
      _ <- info(s"compile rpc ${op.getOperationId}")
      requestType <- compileTypeName(op.getOperationId.capitalize + "Request")
      responseType <- compileTypeName(op.getOperationId.capitalize + "Response")
      operationId <- compileIdentifier(op.getOperationId)
      http = RpcOption.builder(FullIdentifier("google.api.http"))
      _ = http += OptionStatement(Identifier(method.toString.toLowerCase), path)

      requestBuilder = Message.builder(requestType)
      validParams <- if (op.getParameters != null) {
        op.getParameters.asScala.toVector
          .traverse[Result, Either[ExitCode, Unit]] { p =>
            (for {
              _ <- info(s"compile parameter: ${p.getName}")
              _ <- require(p.getName != null, "parameter name is required")
              _ <- require(p.getName.nonEmpty, "parameter name is required")
              _ <- require(p.getSchema != null, "parameter schema is required")
              _ <- requireNotRef(p.getSchema)
              _ <- compileField(requestBuilder, p.getName, p.getSchema, p.getExtensions, p.getRequired)
            } yield ()).attempt
          }
          .map(_.forall(_.isRight))
      } else info("no query parameters body").map(_ => true)
      validRequest <- if (op.getRequestBody != null)
        for {
          _ <- require(op.getRequestBody.get$ref() == null, "request body $ref not allowed")
          _ <- require(op.getRequestBody.getContent != null, "request body content required")
          _ <- info(s"compile request body")
          result <- compileContent(
            http,
            requestBuilder,
            op.getRequestBody.getContent,
            "request_body",
            Identifier("body")
          ).attempt
        } yield result.isRight
      else info("no request body").map(_ => true)
      _ = protoFile += requestBuilder.build

      responseBuilder = Message.builder(responseType)
      _ <- require(op.getResponses != null, "responses is required")
      responses = op.getResponses
      response = if (responses.containsKey("default")) {
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
      validResponse <- response match {
        case None => info("no response").map(_ => true)
        case Some(response) =>
          for {
            _ <- info("compile response")
            _ <- require(response.get$ref() == null, "")
            _ <- if (response.getContent == null) info("no response body").map(_ => true)
            else
              for {
                _ <- info("compile response body")
                result <- compileContent(
                  http,
                  responseBuilder,
                  response.getContent,
                  "response_body",
                  Identifier("response_body")
                ).attempt
              } yield result.isRight
          } yield true
      }

      _ = protoFile += responseBuilder.build

      _ <- require(validParams, "has invalid request params")
      _ <- require(validRequest, "has invalid request body")
      _ <- require(validResponse, "has invalid response")

      rpc = RpcStatement.builder(operationId)
      _ = rpc.withRequestType(requestType)
      _ = rpc.withResponseType(responseType)
      _ = rpc += http.build
      _ = service += rpc.build
    } yield ()

  def compileContent(
    options: RpcOptionBuilder,
    builder: MessageBuilder,
    content: Content,
    fieldName: String,
    context: Identifier
  ): Result[Unit] =
    for {
      _ <- info("compile content")
      _ <- require(!content.isEmpty, s"$context content should not be empty")
      _ <- require(content.size() == 1, s"$context content should contain only one media type")
      (mediaType, media) = content.asScala.head
      _ <- require(media != null, "media type is required")
      schema = media.getSchema
      _ <- require(schema != null, "media type schema is required")

      fieldId <- compileFieldIdentifier(media.getExtensions, fieldName)
      fieldNum = util.extension[Int](media.getExtensions, "x-proto-field-id").getOrElse(builder.nextFieldNum)
      _ = mediaType match {
        case "application/json" =>
          for {
            field <- compileField(fieldId, fieldNum, schema, required = true)
            _ = builder += field
          } yield ()
        case "text/plain" =>
          for {
            _ <- require(schema.getType == "string", s"$fieldName: schema type should be string")
            _ = builder += NormalField(Identifier("string"), fieldId, fieldNum, Vector.empty)
          } yield ()
        case "application/octet-stream" =>
          for {
            _ <- require(schema.getType == "string", s"$fieldName: schema type should be string")
            _ <- require(schema.getFormat == "binary", s"$fieldName: string format should be binary")
            _ = builder += NormalField(Identifier("bytes"), fieldId, fieldNum, Vector.empty)
          } yield ()
      }
      _ = options += OptionStatement(context, fieldId.value)
    } yield ()

  def compileField(
    builder: MessageBuilder,
    fiendName: String,
    schema: Schema[_],
    extensions: Extensions,
    required: Boolean
  ): Result[Unit] =
    for {
      _ <- info(s"compile field: $fiendName")
      fieldIdentifier <- compileFieldIdentifier(extensions, fiendName)
      fieldNum = util.extension[Int](extensions, "x-proto-field-id").getOrElse(builder.nextFieldNum)
      field <- compileField(fieldIdentifier, fieldNum, schema, required)
      _ = builder += field
      _ <- info(s"compile field: $fiendName done")
    } yield ()

  def compileField(fieldId: Identifier, fieldNum: Int, schema: Schema[_], required: Boolean): Result[MessageField] =
    schema match {
      case integer: IntegerSchema        => compileInteger(integer, required).map(NormalField(_, fieldId, fieldNum))
      case number: NumberSchema          => compileNumber(number, required).map(NormalField(_, fieldId, fieldNum))
      case string: StringSchema          => compileString(string, required).map(NormalField(_, fieldId, fieldNum))
      case date: DateSchema              => compileDate(date, required).map(NormalField(_, fieldId, fieldNum))
      case date: DateTimeSchema          => compileDateTime(date, required).map(NormalField(_, fieldId, fieldNum))
      case boolean: BooleanSchema        => compileBoolean(boolean, required).map(NormalField(_, fieldId, fieldNum))
      case _ if schema.get$ref() != null => compileComponentRef(schema).map(NormalField(_, fieldId, fieldNum))
      case array: ArraySchema            => compileArrayType(array).map(RepeatedField(_, fieldId, fieldNum))
      case _                             => error("schema is not supported")
    }

  def compileFieldIdentifier(extensions: Extensions, fieldName: String): Result[Identifier] =
    util.extension[String](extensions, "x-proto-field") match {
      case Some(value) => compileIdentifier(util.camelCaseToUnderscore(util.cleanup(value)))
      case None =>
        for {
          _ <- info("x-proto-field is not defined")
          id <- compileIdentifier(util.camelCaseToUnderscore(util.cleanup(fieldName)))
        } yield id
    }

  def compileInteger(schema: IntegerSchema, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- info("compile integer")
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      format <- (required, Option(schema.getFormat)) match {
        case (true, Some("int32"))  => compileIdentifier("int32")
        case (true, Some("int64"))  => compileIdentifier("int64")
        case (true, None)           => compileIdentifier("int64")
        case (false, Some("int32")) => compileFullIdentifier("google.protobuf.Int32Value")
        case (false, Some("int64")) => compileFullIdentifier("google.protobuf.Int64Value")
        case (false, None)          => compileFullIdentifier("google.protobuf.Int64Value")
        case (_, Some(format))      => error(s"unexpected format: $format")
      }
    } yield format

  def compileNumber(schema: NumberSchema, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- info("compile number")
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      format <- (required, Option(schema.getFormat)) match {
        case (true, Some("float"))   => compileIdentifier("float")
        case (true, Some("double"))  => compileIdentifier("double")
        case (true, None)            => compileIdentifier("double")
        case (false, Some("float"))  => compileFullIdentifier("google.protobuf.FloatValue")
        case (false, Some("double")) => compileFullIdentifier("google.protobuf.DoubleValue")
        case (false, None)           => compileFullIdentifier("google.protobuf.DoubleValue")
        case (_, Some(format))       => error(s"unexpected format: $format")
      }
    } yield format

  def compileString(schema: StringSchema, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- info("compile string")
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      _ <- requireNoFormat(schema)
    } yield
      if (required) Identifier("string")
      else FullIdentifier("google.protobuf.StringValue")

  def compileDate(schema: DateSchema, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- info("compile date")
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
    } yield
      if (required) Identifier("string")
      else FullIdentifier("google.protobuf.StringValue")

  def compileDateTime(schema: DateTimeSchema, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- info("compile datetime")
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
    } yield
      if (required) Identifier("string")
      else FullIdentifier("google.protobuf.StringValue")

  def compileBoolean(schema: BooleanSchema, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- info("compile boolean")
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
    } yield
      if (required) Identifier("bool")
      else FullIdentifier("google.protobuf.BoolValue")

  def compileComponentRef(schema: Schema[_]): Result[TypeIdentifier] =
    for {
      _ <- info("compile component ref")
      _ <- requireNotEnum(schema)
      typeName <- schema.get$ref() match {
        case util.schemaRef(name) => compileTypeName(name)
        case _                    => error(s"unsupported ref: ${schema.get$ref()}")
      }
    } yield typeName

  def compileArrayType(schema: ArraySchema): Result[TypeIdentifier] =
    for {
      _ <- info("compile array type")
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      _ <- require(schema.getItems != null, "array items schema is required")
      _ <- requireNotEnum(schema.getItems)
      items = schema.getItems
      format <- items match {
        case integer: IntegerSchema       => compileInteger(integer, required = true)
        case number: NumberSchema         => compileNumber(number, required = true)
        case string: StringSchema         => compileString(string, required = true)
        case date: DateSchema             => compileDate(date, required = true)
        case date: DateTimeSchema         => compileDateTime(date, required = true)
        case boolean: BooleanSchema       => compileBoolean(boolean, required = true)
        case _: ArraySchema               => error("nested array schema is not supported")
        case _ if items.get$ref() != null => compileComponentRef(schema.getItems)
        case _                            => error(s"items schema is not supported: $items")
      }
    } yield format

  def requireNotRef(schema: Schema[_]): Result[Unit] =
    require(schema.get$ref() == null, "$ref is not allowed")

  def requireNotEnum(schema: Schema[_]): Result[Unit] =
    require(schema.getEnum == null, "enum is not allowed")

  def requireNoFormat(schema: Schema[_]): Result[Unit] =
    require(schema.getFormat == null, "format is not allowed")

  def require(test: Boolean, message: String): Result[Unit] =
    if (test) EitherT.pure(()) else error(message)
}
