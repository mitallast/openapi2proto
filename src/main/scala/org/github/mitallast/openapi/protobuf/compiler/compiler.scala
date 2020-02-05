package org.github.mitallast.openapi.protobuf.compiler

import java.nio.file.Paths

import cats.data._
import cats.effect.ExitCode
import cats.implicits._
import org.github.mitallast.openapi.protobuf.common._
import org.github.mitallast.openapi.protobuf.parser.{Scalar, _}
import org.github.mitallast.openapi.protobuf.model._
import org.yaml.snakeyaml.nodes.{MappingNode, Node, ScalarNode, SequenceNode}

import scala.jdk.CollectionConverters._

object extractors {
  object IntegerSchema {
    def unapply(value: Schema): Option[SchemaNormal] =
      value match {
        case schema: SchemaNormal =>
          schema.`type` match {
            case Some(SchemaType.INTEGER_TYPE) => Some(schema)
            case _                             => None
          }
        case _ => None
      }
  }
  object NumberSchema {
    def unapply(value: Schema): Option[SchemaNormal] =
      value match {
        case schema: SchemaNormal =>
          schema.`type` match {
            case Some(SchemaType.NUMBER_TYPE) => Some(schema)
            case _                            => None
          }
        case _ => None
      }
  }
  object StringSchema {
    def unapply(value: Schema): Option[SchemaNormal] =
      value match {
        case schema: SchemaNormal =>
          schema.`type` match {
            case Some(SchemaType.STRING_TYPE)
                if (schema.format.isEmpty
                  && schema.enum.isEmpty) =>
              Some(schema)
            case _ => None
          }
        case _ => None
      }
  }
  object BinaryStringSchema {
    def unapply(value: Schema): Option[SchemaNormal] =
      value match {
        case schema: SchemaNormal =>
          schema.`type` match {
            case Some(SchemaType.STRING_TYPE)
                if (schema.format.contains(FormatType.BINARY_FORMAT)
                  && schema.enum.isEmpty) =>
              Some(schema)
            case _ => None
          }
        case _ => None
      }
  }
  object DateSchema {
    def unapply(value: Schema): Option[SchemaNormal] =
      value match {
        case schema: SchemaNormal =>
          schema.`type` match {
            case Some(SchemaType.STRING_TYPE)
                if (schema.format.contains(FormatType.DATE_FORMAT)
                  && schema.enum.isEmpty) =>
              Some(schema)
            case _ => None
          }
        case _ => None
      }
  }
  object DateTimeSchema {
    def unapply(value: Schema): Option[SchemaNormal] =
      value match {
        case schema: SchemaNormal =>
          schema.`type` match {
            case Some(SchemaType.STRING_TYPE)
                if (schema.format.contains(FormatType.DATE_TIME_FORMAT) && schema.enum.isEmpty) =>
              Some(schema)
            case _ => None
          }
        case _ => None
      }
  }
  object BooleanSchema {
    def unapply(value: Schema): Option[SchemaNormal] =
      value match {
        case schema: SchemaNormal =>
          schema.`type` match {
            case Some(SchemaType.BOOLEAN_TYPE) => Some(schema)
            case _                             => None
          }
        case _ => None
      }
  }
  object ArraySchema {
    def unapply(value: Schema): Option[SchemaNormal] =
      value match {
        case schema: SchemaNormal =>
          schema.`type` match {
            case Some(SchemaType.ARRAY_TYPE) => Some(schema)
            case _                           => None
          }
        case _ => None
      }
  }
  object OneOfSchema {
    def unapply(value: Schema): Option[SchemaNormal] =
      value match {
        case schema: SchemaNormal
            if (schema.allOf.isEmpty
              && schema.anyOf.isEmpty
              && schema.oneOf.nonEmpty) =>
          Some(schema)
        case _ => None
      }
  }
  object ObjectSchema {
    def unapply(value: Schema): Option[SchemaNormal] =
      value match {
        case schema: SchemaNormal if schema.`type`.contains(SchemaType.OBJECT_TYPE) => Some(schema)
        case _                                                                      => None
      }
  }

  object EnumSchema {
    def unapply(value: Schema): Option[SchemaNormal] =
      value match {
        case schema: SchemaNormal if schema.enum.nonEmpty => Some(schema)
        case _                                            => None
      }
  }

  object BooleanScalar {
    def unapply(scalar: ScalarNode): Option[Boolean] =
      if (scalar.isPlain) scalar.getValue.toBooleanOption else None
  }

  object LongScalar {
    def unapply(scalar: ScalarNode): Option[Long] =
      if (scalar.isPlain) scalar.getValue.toLongOption else None
  }

  object DoubleScalar {
    def unapply(scalar: ScalarNode): Option[Double] =
      if (scalar.isPlain) scalar.getValue.toDoubleOption else None
  }
}

object ProtoCompiler {
  import extractors._

  def compile(api: OpenAPI, resolver: Resolver): Result[ProtoFile] =
    for {
      packageName <- compilePackageName(api)
      protoPath <- compileProtoPath(api)
      protoFile = ProtoFile.builder(protoPath, packageName)
      _ = protoFile += ImportStatement("google/api/annotations.proto")
      _ = protoFile += ImportStatement("google/protobuf/wrappers.proto")
      _ = protoFile += ImportStatement("google/protobuf/timestamp.proto")
      importsValid <- compileImports(protoFile, api)
      optionsValid <- compileOptions(protoFile, api)
      componentsValid <- compileComponents(protoFile, api, resolver)
      serviceValid <- compileService(protoFile, api, resolver)
      resolvedValid <- compileResolved(protoFile, resolver)
      _ <- if (importsValid && optionsValid && componentsValid && serviceValid && resolvedValid) unit else left
    } yield protoFile.build

  def compileImports(protoFile: ProtoFileBuilder, api: OpenAPI): Result[Boolean] =
    for {
      imports <- compileExtensionStringSeq(api.extensions, "x-proto-import")
      result <- imports
        .traverse[Result, Unit] { importFile =>
          for {
            valid <- delay(lexical.validate(importFile.value, lexical.identifiers.protoPath))
            _ <- require(valid, importFile.node, "not valid path")
            _ = protoFile += ImportStatement(importFile.value)
          } yield ()
        }
        .attempt
    } yield result.isRight

  def compileOptions(protoFile: ProtoFileBuilder, api: OpenAPI): Result[Boolean] =
    for {
      options <- compileExtensionMap(api.extensions, "x-proto-option")
      result <- options.entries
        .traverse[Result, Unit] {
          case (key, valueNode) =>
            for {
              optionId <- compileFullIdentifier(key.node, key.value)
              scalar <- requireScalarNode(valueNode)
              constant = scalar match {
                case BooleanScalar(value) => BooleanValue(value)
                case LongScalar(value)    => LongValue(value)
                case DoubleScalar(value)  => DoubleValue(value)
                case _                    => StringValue(scalar.getValue)
              }
              _ = protoFile += OptionStatement(optionId, constant)
            } yield ()
        }
        .attempt
    } yield result.isRight

  def compileExtensionMap(ext: Extensions, key: String): Result[ScalarMap[String, Node]] =
    (for {
      value <- OptionT.fromOption[Result](ext.get(key))
      fields <- value match {
        case mappingNode: MappingNode =>
          OptionT.liftF {
            mappingNode.getValue.asScala.toVector
              .traverse[Result, (Scalar[String], Node)] { tuple =>
                for {
                  scalar <- requireScalarNode(tuple.getKeyNode)
                } yield (Scalar(scalar, scalar.getValue), tuple.getValueNode)
              }
          }
        case _ => OptionT.liftF(error[Vector[(Scalar[String], Node)]](value, "expected object"))
      }
    } yield ScalarMap(fields)).value.map(_.getOrElse(ScalarMap.empty))

  def requireScalarNode(node: Node): Result[ScalarNode] =
    node match {
      case s: ScalarNode => pure(s)
      case _             => error(node, s"expected scalar, actual ${node.getTag}")
    }

  def compileExtensionString(ext: Extensions, key: String): Result[Option[Scalar[String]]] =
    (for {
      value <- OptionT.fromOption[Result](ext.get(key))
      scalar <- value match {
        case s: ScalarNode => OptionT.pure[Result](s)
        case _             => OptionT.liftF(error[ScalarNode](value, "expected scalar"))
      }
    } yield Scalar(scalar, scalar.getValue)).value

  def compileExtensionStringSeq(ext: Extensions, key: String): Result[Vector[Scalar[String]]] =
    (for {
      value <- OptionT.fromOption[Result](ext.get(key))
      sequenceNode <- value match {
        case s: SequenceNode => OptionT.pure[Result](s)
        case _               => OptionT.liftF(error[SequenceNode](value, "expected sequence"))
      }
      values <- OptionT.liftF(sequenceNode.getValue.asScala.toVector.traverse[Result, Scalar[String]] { node =>
        for {
          scalar <- node match {
            case scalar: ScalarNode => pure(scalar)
            case _                  => error[ScalarNode](node, "expected scalar")
          }
        } yield Scalar(scalar, scalar.getValue)
      })
    } yield values).value.map(_.getOrElse(Vector.empty))

  def compilePackageName(api: OpenAPI): Result[FullIdentifier] =
    for {
      opt <- compileExtensionString(api.extensions, "x-proto-package")
      id <- opt match {
        case Some(value) => compileFullIdentifier(value.node, value.value)
        case None =>
          for {
            _ <- warning(api.node, "x-proto-package is not defined")
            raw = util.cleanup(api.filepath.getFileName.toString.replaceAll("\\.[a-zA-Z]{3,5}$", ""))
            id <- compileFullIdentifier(api.node, raw)
          } yield id
      }
    } yield id

  def compileProtoPath(api: OpenAPI): Result[ProtoPath] =
    for {
      protoPath <- delay {
        val parent = api.filepath.getParent
        val filename = api.filepath.getFileName
        val protoFilename = filename.toString.replaceAll("\\.ya?ml$", "") + ".proto"
        parent.resolve(protoFilename).toString
      }
    } yield ProtoPath(protoPath)

  def compileReserved(schema: SchemaNormal): Result[Vector[Int]] =
    for {
      seq <- compileExtensionStringSeq(schema.extensions, "x-proto-reserved")
      reserved <- seq.traverse[Result, Int] { constant =>
        if (constant.value.matches("^\\d+$")) {
          pure(constant.value.toInt)
        } else {
          error(constant.node, "expected integer")
        }
      }
    } yield reserved

  def compileSchemaRef(api: OpenAPI, $ref: SchemaReference, resolver: Resolver): Result[Schema] =
    $ref match {
      case ComponentsReference(id) =>
        for {
          _ <- require(api.components.schemas.contains(id), id.node, s"component schema $id does not exists")
        } yield api.components.schemas(id)
      case ExternalReference(file, ComponentsReference(id)) =>
        for {
          externalApi <- resolver.resolve(api.filepath, file)
          schemas = externalApi.components.schemas
          _ <- require(schemas.contains(id), id.node, s"component schema $file $id does not exists")
        } yield schemas(id)
      case _ => error($ref.id.node, s"unsupported ref: ${$ref}")
    }

  def compileFullIdentifier(node: Node, value: String): Result[FullIdentifier] =
    if (lexical.validate(value, lexical.identifiers.fullIdent)) {
      pure(FullIdentifier(value))
    } else error(node, "not valid full identifier")

  def compileTypeName(value: Scalar[String]): Result[Identifier] =
    compileIdentifier(value.map(util.cleanup).map(util.underscoreToCamelCase).map(_.capitalize))

  def compileIdentifier(value: Scalar[String]): Result[Identifier] =
    if (lexical.validate(value.value, lexical.identifiers.ident)) {
      EitherT.pure(Identifier(value.value))
    } else error(value.node, "not valid identifier")

  def compileResolved(protoFile: ProtoFileBuilder, resolver: Resolver): Result[Boolean] =
    for {
      resolved <- resolver.resolved()
      valid <- resolved
        .traverse[Result, Boolean] { api =>
          compileComponents(protoFile, api, resolver)
        }
        .map { results =>
          results.forall(_ == true)
        }
    } yield valid

  def compileComponents(protoFile: ProtoFileBuilder, api: OpenAPI, resolver: Resolver): Result[Boolean] =
    for {
      valid <- api.components.schemas.toVector
        .traverse[Result, Either[ExitCode, Unit]] {
          case (typeName, EnumSchema(schema)) =>
            compileComponentEnum(protoFile, typeName, schema).attempt
          case (typeName, ObjectSchema(schema)) =>
            compileComponentObject(protoFile, typeName, api, schema, resolver).attempt
          case (typeName, schema) =>
            warning(schema.node, s"ignore component: $typeName").attempt
        }
        .map(_.forall(_.isRight))
    } yield valid

  def compileComponentObject(
    protoFile: ProtoFileBuilder,
    typeName: Scalar[String],
    api: OpenAPI,
    schema: SchemaNormal,
    resolver: Resolver
  ): Result[Unit] =
    for {
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      typeIdentifier <- compileIdentifier(typeName)
      _ <- require(!protoFile.contains(typeIdentifier), typeName.node, "already defined")
      messageBuilder = Message.builder(typeIdentifier)
      reserved <- compileReserved(schema)
      _ = messageBuilder.reserved(reserved)
      requiredFields = schema.required.map(_.value).toSet
      valid <- schema.properties.toVector
        .traverse[Result, Either[ExitCode, Unit]] {
          case (fieldName, schema) =>
            val required = requiredFields.contains(fieldName.value)
            compileField(messageBuilder, fieldName, api, schema, schema.extensions, required, resolver).attempt
        }
        .map(_.forall(_.isRight))
      _ = protoFile += messageBuilder.build
      _ <- if (valid) unit else left
    } yield ()

  def compileEnumWrapper(schema: SchemaNormal, enumId: Identifier): Result[Identifier] =
    for {
      xWrapper <- compileExtensionString(schema.extensions, "x-proto-wrapper")
      wrapperId <- xWrapper match {
        case Some(wrapperType) => compileTypeName(wrapperType)
        case None              => pure(enumId)
      }
    } yield wrapperId

  def compileComponentEnum(protoFile: ProtoFileBuilder, typeName: Scalar[String], schema: SchemaNormal): Result[Unit] =
    for {
      _ <- requireNoFormat(schema)
      enumId <- compileTypeName(typeName)
      enumBuilder = Enum.builder(enumId)
      _ <- schema.`enum`
        .traverse[Result, Either[ExitCode, Unit]] { constant =>
          val formatted = constant.map(util.cleanup).map {
            case util.constantRegex(value)  => value.toUpperCase()
            case util.camelCaseRegex(value) => util.camelCaseToUnderscore(value).toUpperCase()
            case value                      => util.camelCaseToUnderscore(value).toUpperCase()
          }
          (for {
            id <- compileIdentifier(formatted)
            value = enumBuilder.nextValueNum
            _ <- require(!enumBuilder.contains(value), constant.node, s"Value $value already used")
            _ = enumBuilder += EnumValue(id, value, Vector.empty)
          } yield ()).attempt
        }
        .map(_.forall(_.isRight))
      wrapperId <- compileEnumWrapper(schema, enumId)
      _ <- require(!protoFile.contains(wrapperId), typeName.node, s"$wrapperId already defined")
      wrapperBuilder = Message.builder(wrapperId)
      _ = wrapperBuilder += enumBuilder.build
      _ = protoFile += wrapperBuilder.build
    } yield ()

  def compileServiceName(api: OpenAPI): Result[Identifier] =
    for {
      opt <- compileExtensionString(api.extensions, "x-proto-service")
      id <- opt match {
        case Some(value) => compileIdentifier(value)
        case None =>
          for {
            _ <- warning(api.node, "x-proto-service is not defined")
            id <- compileTypeName(api.info.title.map(_ + "Service"))
          } yield id
      }
    } yield id

  def compileService(protoFile: ProtoFileBuilder, api: OpenAPI, resolver: Resolver): Result[Boolean] =
    for {
      serviceName <- compileServiceName(api)
      serviceBuilder = Service.builder(serviceName)
      valid <- api.paths.values.toVector
        .flatMap {
          case (path, item) =>
            item.operations.map {
              case (method, op) =>
                (path, method, op)
            }
        }
        .traverse[Result, Either[ExitCode, Unit]] {
          case (path, method, op) =>
            compileRpc(protoFile, serviceBuilder, method, path, op, api, resolver).attempt
        }
        .map(_.forall(_.isRight))
      _ = protoFile += serviceBuilder.build
    } yield valid

  def compileRpc(
    protoFile: ProtoFileBuilder,
    service: ServiceBuilder,
    method: HttpMethod,
    path: Scalar[String],
    op: Operation,
    api: OpenAPI,
    resolver: Resolver
  ): Result[Unit] =
    for {
      _ <- require(op.operationId.isDefined, op.node, "require operationId")
      operationId = op.operationId.get

      xRequestType <- compileExtensionString(op.extensions, "x-proto-request")
      requestType <- xRequestType match {
        case Some(typeName) => compileTypeName(typeName)
        case None           => compileTypeName(operationId.map(_.capitalize + "Request"))
      }
      _ <- require(!protoFile.contains(requestType), operationId.node, s"$requestType already defined")

      xResponseType <- compileExtensionString(op.extensions, "x-proto-response")
      responseType <- xResponseType match {
        case Some(typeName) => compileTypeName(typeName)
        case None           => compileTypeName(operationId.map(_.capitalize + "Response"))
      }
      _ <- require(!protoFile.contains(responseType), operationId.node, s"$responseType already defined")

      operationId <- compileIdentifier(operationId)
      http = RpcOption.builder(FullIdentifier.http)
      _ = http += OptionStatement(Identifier(method.toString.toLowerCase), path.value)

      requestBuilder = Message.builder(requestType)
      validParams <- op.parameters
        .traverse[Result, Either[ExitCode, Unit]] { p =>
          (for {
            _ <- require(p.name.value.nonEmpty, p.node, "parameter name is required")
            _ <- require(p.schema.isDefined, p.node, "parameter schema is required")
            schema = p.schema.get
            _ <- requireNotRef(schema)
            _ <- compileField(requestBuilder, p.name, api, schema, p.extensions, p.required.exists(_.value), resolver)
          } yield ()).attempt
        }
        .map(_.forall(_.isRight))
      validRequest <- op.requestBody match {
        case None => pure(true)
        case Some(requestBody) =>
          compileRequestContent(http, requestBuilder, requestBody, api, resolver).attempt
            .map(_.isRight)
      }
      _ = protoFile += requestBuilder.build

      responseBuilder = Message.builder(responseType)
      response = if (op.responses.values.contains("default")) {
        val response = op.responses.values("default")
        Some(response)
      } else if (op.responses.values.contains("200")) {
        val response = op.responses.values("200")
        Some(response)
      } else if (op.responses.values.contains("201")) {
        val response = op.responses.values("201")
        Some(response)
      } else {
        None
      }
      validResponse <- response match {
        case None => pure(true)
        case Some(response) =>
          for {
            valid <- if (response.content.isEmpty) pure(true)
            else
              compileResponseContent(http, responseBuilder, response, api, resolver).attempt
                .map(_.isRight)
          } yield valid
      }
      _ = protoFile += responseBuilder.build
      _ <- if (validParams && validRequest && validResponse) unit else left
      rpc = RpcStatement.builder(operationId)
      _ = rpc.withRequestType(requestType)
      _ = rpc.withResponseType(responseType)
      _ = rpc += http.build
      _ = service += rpc.build
    } yield ()

  def compileRequestContent(
    options: RpcOptionBuilder,
    builder: MessageBuilder,
    requestBody: RequestBody,
    api: OpenAPI,
    resolver: Resolver
  ): Result[Unit] =
    for {
      fieldName <- pure(Scalar(requestBody.node, "request_body"))
      content <- pure(requestBody.content)
      _ <- require(content.media.nonEmpty, content.node, "request content should not be empty")
      _ <- require(content.media.size == 1, content.node, "request content should contain only one media type")
      (mediaType, media) = content.media.entries.head
      _ <- require(media.schema.isDefined, media.node, "request media type schema is required")
      schema = media.schema.get
      _ <- mediaType.value match {
        case "application/json" =>
          // The name of the request field whose value is mapped to the HTTP request
          // body, or `*` for mapping all request fields not captured by the path
          // pattern to the HTTP body, or omitted for not having any HTTP request body.
          schema match {
            case ObjectSchema(objectSchema) =>
              for {
                _ <- compileInlineObject(builder, objectSchema, api, resolver)
                _ = options += OptionStatement(Identifier.body, "*")
              } yield ()
            case _ =>
              for {
                fieldId <- compileFieldIdentifier(media.extensions, fieldName)
                _ <- compileField(builder, fieldId, api, schema, media.extensions, required = true, resolver)
                _ = options += OptionStatement(Identifier.body, fieldId.value)
              } yield ()
          }
        case "text/plain" =>
          for {
            _ <- schema match {
              case StringSchema(_) => unit
              case _               => error(schema.node, s"$fieldName: schema type should be string")
            }
            fieldNum <- compileFieldNum(builder, media.extensions)
            fieldId <- compileFieldIdentifier(media.extensions, fieldName)
            _ = builder += NormalField(Identifier.string, fieldId, fieldNum, Vector.empty)
            _ = options += OptionStatement(Identifier.body, fieldId.value)
          } yield ()
        case "application/octet-stream" =>
          for {
            _ <- schema match {
              case BinaryStringSchema(_) => unit
              case _                     => error(schema.node, s"$fieldName: schema type should be binary string")
            }
            fieldNum <- compileFieldNum(builder, media.extensions)
            fieldId <- compileFieldIdentifier(media.extensions, fieldName)
            _ = builder += NormalField(Identifier.bytes, fieldId, fieldNum, Vector.empty)
            _ = options += OptionStatement(Identifier.body, fieldId.value)
          } yield ()
        case _ =>
          error(mediaType.node, "unexpected media type")
      }
    } yield ()

  def compileResponseContent(
    options: RpcOptionBuilder,
    builder: MessageBuilder,
    response: ApiResponse,
    api: OpenAPI,
    resolver: Resolver
  ): Result[Unit] =
    for {
      fieldName <- pure(Scalar(response.node, "response_body"))
      content <- pure(response.content.get)
      _ <- require(content.media.nonEmpty, content.node, "response content should not be empty")
      _ <- require(content.media.size == 1, content.node, "response content should contain only one media type")
      (mediaType, media) = content.media.entries.head
      _ <- require(media.schema.isDefined, media.node, "response media type schema is required")
      schema = media.schema.get
      _ <- mediaType.value match {
        case "application/json" =>
          schema match {
            case ObjectSchema(objectSchema) =>
              // When option `response_body` omitted, the entire response message
              // will be used as the HTTP response body.
              for {
                _ <- compileInlineObject(builder, objectSchema, api, resolver)
              } yield ()
            case _ =>
              for {
                fieldId <- compileFieldIdentifier(media.extensions, fieldName)
                _ <- compileField(builder, fieldId, api, schema, media.extensions, required = true, resolver)
                _ = options += OptionStatement(Identifier.response_body, fieldId.value)
              } yield ()
          }
        case "text/plain" =>
          for {
            _ <- schema match {
              case StringSchema(_) => unit
              case _               => error(schema.node, s"$fieldName: schema type should be string")
            }
            fieldNum <- compileFieldNum(builder, media.extensions)
            fieldId <- compileFieldIdentifier(media.extensions, fieldName)
            _ = builder += NormalField(Identifier.string, fieldId, fieldNum, Vector.empty)
            _ = options += OptionStatement(Identifier.response_body, fieldId.value)
          } yield ()
        case "application/octet-stream" =>
          for {
            _ <- schema match {
              case BinaryStringSchema(_) => unit
              case _                     => error(schema.node, s"$fieldName: schema type should be binary string")
            }
            fieldNum <- compileFieldNum(builder, media.extensions)
            fieldId <- compileFieldIdentifier(media.extensions, fieldName)
            _ = builder += NormalField(Identifier.bytes, fieldId, fieldNum, Vector.empty)
            _ = options += OptionStatement(Identifier.response_body, fieldId.value)
          } yield ()
        case _ =>
          error(mediaType.node, "unexpected media type")
      }
    } yield ()

  def compileInlineObject(
    messageBuilder: MessageBuilder,
    schema: SchemaNormal,
    api: OpenAPI,
    resolver: Resolver
  ): Result[Boolean] =
    for {
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      reserved <- compileReserved(schema)
      _ = messageBuilder.reserved(reserved)
      requiredFields = schema.required.map(_.value).toSet
      valid <- schema.properties.toVector
        .traverse[Result, Either[ExitCode, Unit]] {
          case (fieldName, schema) =>
            val required = requiredFields.contains(fieldName.value)
            compileField(messageBuilder, fieldName, api, schema, schema.extensions, required, resolver).attempt
        }
        .map(_.forall(_.isRight))
    } yield valid

  def compileField(
    builder: MessageBuilder,
    fiendName: Scalar[String],
    api: OpenAPI,
    schema: Schema,
    extensions: Extensions,
    required: Boolean,
    resolver: Resolver
  ): Result[Unit] =
    for {
      fieldIdentifier <- compileFieldIdentifier(extensions, fiendName)
      _ <- compileField(builder, fieldIdentifier, api, schema, extensions, required, resolver)
    } yield ()

  def compileField(
    builder: MessageBuilder,
    fieldId: Identifier,
    api: OpenAPI,
    schema: Schema,
    extensions: Extensions,
    required: Boolean,
    resolver: Resolver
  ): Result[Unit] =
    schema match {
      case IntegerSchema(integer) =>
        for {
          fieldType <- compileInteger(integer, required)
          fieldNum <- compileFieldNum(builder, extensions)
          _ = builder += NormalField(fieldType, fieldId, fieldNum)
        } yield ()
      case NumberSchema(number) =>
        for {
          fieldType <- compileNumber(number, required)
          fieldNum <- compileFieldNum(builder, extensions)
          _ = builder += NormalField(fieldType, fieldId, fieldNum)
        } yield ()
      case StringSchema(string) =>
        for {
          fieldType <- compileString(string, required)
          fieldNum <- compileFieldNum(builder, extensions)
          _ = builder += NormalField(fieldType, fieldId, fieldNum)
        } yield ()
      case DateSchema(date) =>
        for {
          fieldType <- compileDate(date, required)
          fieldNum <- compileFieldNum(builder, extensions)
          _ = builder += NormalField(fieldType, fieldId, fieldNum)
        } yield ()
      case DateTimeSchema(date) =>
        for {
          fieldType <- compileDateTime(date, required)
          fieldNum <- compileFieldNum(builder, extensions)
          _ = builder += NormalField(fieldType, fieldId, fieldNum)
        } yield ()
      case BooleanSchema(boolean) =>
        for {
          fieldType <- compileBoolean(boolean, required)
          fieldNum <- compileFieldNum(builder, extensions)
          _ = builder += NormalField(fieldType, fieldId, fieldNum)
        } yield ()
      case reference: Reference =>
        for {
          fieldType <- compileComponentRef(api, reference, resolver)
          fieldNum <- compileFieldNum(builder, extensions)
          _ = builder += NormalField(fieldType, fieldId, fieldNum)
        } yield ()
      case ArraySchema(array) =>
        for {
          fieldType <- compileArrayType(api, array, resolver)
          fieldNum <- compileFieldNum(builder, extensions)
          _ = builder += RepeatedField(fieldType, fieldId, fieldNum)
        } yield ()
      case OneOfSchema(composed) =>
        for {
          _ <- requireNotRef(composed)
          _ <- requireNotEnum(composed)
          oneOf = OneOf.builder(fieldId)
          valid <- composed.oneOf
            .traverse[Result, Either[ExitCode, Unit]] { schema =>
              (for {
                fieldType <- schema match {
                  case IntegerSchema(integer) => compileInteger(integer, required = true)
                  case NumberSchema(number)   => compileNumber(number, required = true)
                  case StringSchema(string)   => compileString(string, required = true)
                  case DateSchema(date)       => compileDate(date, required = true)
                  case DateTimeSchema(date)   => compileDateTime(date, required = true)
                  case BooleanSchema(boolean) => compileBoolean(boolean, required = true)
                  case ref: Reference         => compileComponentRef(api, ref, resolver)
                  case schema                 => error(schema.node, "schema is not supported at oneOf level")
                }
                fieldName = fieldType match {
                  case Identifier(value) =>
                    Scalar(schema.node, util.camelCaseToUnderscore(value))
                  case FullIdentifier(value) =>
                    val typeName = util.extractIdentifier(value)
                    val id = util.camelCaseToUnderscore(typeName)
                    Scalar(schema.node, id)
                }
                fieldId <- compileFieldIdentifier(schema.extensions, fieldName)
                fieldNum <- compileFieldNum(builder, schema.extensions)
                _ = oneOf += OneOfField(fieldType, fieldId, fieldNum, Vector.empty)
              } yield ()).attempt
            }
            .map(_.forall(_.isRight))
          _ = builder += oneOf.build
          _ <- if (valid) unit else left
        } yield ()
      case _ => error(schema.node, s"schema is not supported: $schema")
    }

  def compileFieldNum(builder: MessageBuilder, extensions: Extensions): Result[Int] =
    for {
      opt <- compileExtensionString(extensions, "x-proto-field-id")
      fieldNum <- opt match {
        case Some(constant) =>
          for {
            _ <- require(constant.value.matches("^\\d+$"), constant.node, "expected integer")
            fieldNum = constant.value.toInt
            _ <- require(!builder.contains(fieldNum), constant.node, s"field $fieldNum already defined")
          } yield fieldNum
        case None =>
          pure(builder.nextFieldNum)
      }
      _ = builder.use(fieldNum)
    } yield fieldNum

  def compileFieldIdentifier(extensions: Extensions, fieldName: Scalar[String]): Result[Identifier] =
    for {
      opt <- compileExtensionString(extensions, "x-proto-field")
      constant = opt match {
        case Some(constant) => constant
        case None           => fieldName
      }
      clean = constant.map(util.cleanup).map(util.camelCaseToUnderscore)
      id <- compileIdentifier(clean)
    } yield id

  def compileInteger(schema: SchemaNormal, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      protoFormat <- compileExtensionString(schema.extensions, "x-proto-format")
      isFixed <- protoFormat.map(_.value) match {
        case Some("fixed")  => pure(true)
        case Some("varint") => pure(false)
        case Some(_)        => error(protoFormat.get.node, "unexpected int format")
        case None           => pure(false)
      }
      format <- (required, schema.format, isFixed) match {
        case (true, Some(FormatType.INTEGER32_FORMAT), false)  => pure(Identifier.int32)
        case (true, Some(FormatType.INTEGER32_FORMAT), true)   => pure(Identifier.fixed32)
        case (true, Some(FormatType.INTEGER64_FORMAT), false)  => pure(Identifier.int64)
        case (true, Some(FormatType.INTEGER64_FORMAT), true)   => pure(Identifier.fixed64)
        case (true, None, false)                               => pure(Identifier.int64)
        case (true, None, true)                                => pure(Identifier.fixed64)
        case (false, Some(FormatType.INTEGER32_FORMAT), false) => pure(FullIdentifier.int32)
        case (false, Some(FormatType.INTEGER64_FORMAT), false) => pure(FullIdentifier.int64)
        case (false, None, false)                              => pure(FullIdentifier.int64)
        case (_, Some(format), _)                              => error(schema.node, s"unexpected format: $format")
      }
    } yield format

  def compileNumber(schema: SchemaNormal, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      format <- (required, schema.format) match {
        case (true, Some(FormatType.FLOAT_FORMAT))   => pure(Identifier.float)
        case (true, Some(FormatType.DOUBLE_FORMAT))  => pure(Identifier.double)
        case (true, None)                            => pure(Identifier.double)
        case (false, Some(FormatType.FLOAT_FORMAT))  => pure(FullIdentifier.float)
        case (false, Some(FormatType.DOUBLE_FORMAT)) => pure(FullIdentifier.double)
        case (false, None)                           => pure(FullIdentifier.double)
        case (_, Some(format))                       => error(schema.node, s"unexpected format: $format")
      }
    } yield format

  def compileString(schema: SchemaNormal, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      _ <- requireNoFormat(schema)
    } yield
      if (required) Identifier.string
      else FullIdentifier.string

  def compileDate(schema: SchemaNormal, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
    } yield
      if (required) Identifier.string
      else FullIdentifier.string

  def compileDateTime(schema: SchemaNormal, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      format <- compileExtensionString(schema.extensions, "x-proto-format")
      id <- format.map(_.value) match {
        case Some("unix-time")        => pure(Identifier.fixed64)
        case Some("unix-time-millis") => pure(Identifier.fixed64)
        case Some("timestamp")        => pure(FullIdentifier.timestamp)
        case Some(_)                  => error(format.get.node, "unexpected date format")
        case None                     => if (required) pure(Identifier.string) else pure(FullIdentifier.string)
      }
    } yield id

  def compileBoolean(schema: SchemaNormal, required: Boolean): Result[TypeIdentifier] =
    for {
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
    } yield
      if (required) Identifier.bool
      else FullIdentifier.bool

  def compileComponentRef(api: OpenAPI, schema: Reference, resolver: Resolver): Result[TypeIdentifier] =
    for {
      refSchema <- compileSchemaRef(api, schema.$ref, resolver)
      typeName: TypeIdentifier <- refSchema match {
        case IntegerSchema(integer) => compileInteger(integer, required = true)
        case NumberSchema(number)   => compileNumber(number, required = true)
        case StringSchema(string)   => compileString(string, required = true)
        case EnumSchema(enumSchema) =>
          for {
            enumId <- compileTypeName(schema.$ref.id)
            wrapperId <- compileEnumWrapper(enumSchema, enumId)
          } yield FullIdentifier(wrapperId, enumId)
        case DateSchema(date)       => compileDate(date, required = true)
        case DateTimeSchema(date)   => compileDateTime(date, required = true)
        case BooleanSchema(boolean) => compileBoolean(boolean, required = true)
        case ArraySchema(_)         => error[TypeIdentifier](refSchema.node, "array schema is not supported")
        case ObjectSchema(_)        => compileTypeName(schema.$ref.id)
        case _                      => error[TypeIdentifier](refSchema.node, "schema is not supported")
      }
    } yield typeName

  def compileArrayType(api: OpenAPI, schema: SchemaNormal, resolver: Resolver): Result[TypeIdentifier] =
    for {
      _ <- requireNotRef(schema)
      _ <- requireNotEnum(schema)
      _ <- require(schema.items.isDefined, schema.node, "array items schema is required")
      items = schema.items.get
      format <- items match {
        case ref: Reference         => compileComponentRef(api, ref, resolver)
        case IntegerSchema(integer) => compileInteger(integer, required = true)
        case NumberSchema(number)   => compileNumber(number, required = true)
        case StringSchema(string)   => compileString(string, required = true)
        case DateSchema(date)       => compileDate(date, required = true)
        case DateTimeSchema(date)   => compileDateTime(date, required = true)
        case BooleanSchema(boolean) => compileBoolean(boolean, required = true)
        case ArraySchema(_)         => error(items.node, "nested array schema is not supported")
        case _                      => error(items.node, s"items schema is not supported: $items")
      }
    } yield format

  def requireNotRef(schema: Schema): Result[Unit] =
    require(!schema.isInstanceOf[Reference], schema.node, "$ref is not allowed")

  def requireNotEnum(schema: SchemaNormal): Result[Unit] =
    require(schema.enum.isEmpty, schema.node, "enum is not allowed")

  def requireNoFormat(schema: SchemaNormal): Result[Unit] =
    require(schema.format.isEmpty, schema.node, "format is not allowed")
}
