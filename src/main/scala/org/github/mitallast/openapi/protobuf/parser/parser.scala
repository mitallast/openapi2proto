package org.github.mitallast.openapi.protobuf.parser

import java.io.{FileReader, Reader, StringReader}
import java.nio.file.Path

import cats.data._
import cats.implicits._
import org.yaml.snakeyaml.composer.Composer
import org.yaml.snakeyaml.error.{Mark, MarkedYAMLException, YAMLException}
import org.yaml.snakeyaml.nodes._
import org.yaml.snakeyaml.parser.ParserImpl
import org.yaml.snakeyaml.reader.StreamReader
import org.yaml.snakeyaml.resolver.Resolver
import org.github.mitallast.openapi.protobuf.common._

import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

object NamedStreamReader {
  def apply(filepath: Path): NamedStreamReader = apply(new FileReader(filepath.toString), filepath)
  def apply(reader: Reader, filepath: Path): NamedStreamReader =
    new NamedStreamReader(reader, filepath.getFileName.toString)
}
final class NamedStreamReader(reader: Reader, name: String) extends StreamReader(reader) {
  override def getMark: Mark = {
    val m = super.getMark
    // library stats counting lines from zero, editors from one
    new Mark(name, m.getIndex, m.getLine, m.getColumn, m.getBuffer, m.getPointer)
  }
}

object OpenAPIParser {

  private val schemaRef: Regex = "^#\\/components\\/schemas\\/([a-zA-Z0-9_]+)$".r
  private val externalRef: Regex =
    "^((\\.\\.\\/|[a-z-A-Z0-9_\\.]+\\/)*[a-z-A-Z0-9_\\.]+.ya?ml)#\\/components\\/schemas\\/([a-zA-Z0-9_]+)$".r

  final case class ObjectNode(node: MappingNode, fields: ScalarMap[String, Node]) {
    def allowedFields(names: String*): Result[Unit] =
      for {
        fieldSet <- pure(names.toSet)
        _ <- fields.filterNot(_._1.value.startsWith("x-")).toVector.traverse[Result, Unit] {
          case (field, _) =>
            require(fieldSet.contains(field.value), field.node, s"unexpected field: $field")
        }
      } yield ()

    def parseMap[V](fieldName: String)(f: (Scalar[String], Node) => Result[V]): Result[ScalarMap[String, V]] =
      (for {
        objectNode <- OptionT(optionalObjectField(fieldName))
        map <- OptionT.liftF(objectNode.parseMap(f))
      } yield map).value.map(_.getOrElse(ScalarMap.empty))

    def parseMap[V](f: (Scalar[String], Node) => Result[V]): Result[ScalarMap[String, V]] =
      for {
        entries <- fields.toVector
          .traverse[Result, (Scalar[String], V)] {
            case (field, value) =>
              f.apply(field, value).map((field, _))
          }
      } yield ScalarMap(entries)

    def parseSeq[V](fieldName: String)(f: Node => Result[V]): Result[Vector[V]] =
      (for {
        sequenceNode <- OptionT(optionalSequenceField(fieldName))
        requirements <- OptionT.liftF(sequenceNode.getValue.asScala.toVector.traverse[Result, V](f))
      } yield requirements).value.map(_.getOrElse(Vector.empty))

    def parseStringSeq(fieldName: String): Result[Vector[Scalar[String]]] =
      parseSeq(fieldName) { node =>
        for {
          scalar <- requireScalarNode(node)
        } yield Scalar(scalar, scalar.getValue)
      }

    def parseOptionalObject[V](fieldName: String)(f: ObjectNode => Result[V]): Result[Option[V]] =
      (for {
        node <- OptionT(optionalObjectField(fieldName))
        value <- OptionT.liftF(f(node))
      } yield value).value

    def optionalField(fieldName: String): Result[Option[Node]] =
      pure(fields.get(fieldName))

    def requireField(fieldName: String): Result[Node] =
      for {
        _ <- require(fields.contains(fieldName), node, s"field $fieldName required")
      } yield fields(fieldName)

    def optionalScalarField(fieldName: String): Result[Option[ScalarNode]] =
      (for {
        field <- OptionT(optionalField(fieldName))
        scalar <- OptionT.liftF(requireScalarNode(field))
      } yield scalar).value

    def optionalObjectField(fieldName: String): Result[Option[ObjectNode]] =
      (for {
        field <- OptionT(optionalField(fieldName))
        map <- OptionT.liftF(requireObjectNode(field))
      } yield map).value

    def optionalSequenceField(fieldName: String): Result[Option[SequenceNode]] =
      (for {
        field <- OptionT(optionalField(fieldName))
        seq <- OptionT.liftF(requireSequenceNode(field))
      } yield seq).value

    def requireScalarField(fieldName: String): Result[ScalarNode] =
      for {
        field <- requireField(fieldName)
        scalar <- requireScalarNode(field)
      } yield scalar

    def requireStringConstant(fieldName: String): Result[Scalar[String]] =
      for {
        scalar <- requireScalarField(fieldName)
      } yield Scalar(scalar, scalar.getValue)

    def optionalStringConstant(fieldName: String): Result[Option[Scalar[String]]] =
      (for {
        field <- OptionT(optionalField(fieldName))
        scalar <- OptionT.liftF(requireScalarNode(field))
      } yield Scalar(scalar, scalar.getValue)).value

    def optionalBooleanConstant(fieldName: String): Result[Option[Scalar[Boolean]]] =
      (for {
        field <- OptionT(optionalField(fieldName))
        scalar <- OptionT.liftF(requireScalarNode(field))
      } yield Scalar(scalar, scalar.getValue.toBoolean)).value

    def optionalBigDecimalConstant(fieldName: String): Result[Option[Scalar[BigDecimal]]] =
      (for {
        field <- OptionT(optionalField(fieldName))
        scalar <- OptionT.liftF(requireScalarNode(field))
      } yield Scalar(scalar, BigDecimal(scalar.getValue))).value

    def optionalIntConstant(fieldName: String): Result[Option[Scalar[Int]]] =
      (for {
        field <- OptionT(optionalField(fieldName))
        scalar <- OptionT.liftF(requireScalarNode(field))
      } yield Scalar(scalar, scalar.getValue.toInt)).value

    def requireObjectField(fieldName: String): Result[ObjectNode] =
      for {
        field <- requireField(fieldName)
        map <- requireObjectNode(field)
      } yield map

    def requireSequenceField(fieldName: String): Result[SequenceNode] =
      for {
        field <- requireField(fieldName)
        seq <- requireSequenceNode(field)
      } yield seq

    def extensions: ScalarMap[String, Node] = fields.filter(_._1.value.startsWith("x-"))
  }

  def parse(sources: Map[String, String]): Result[Map[Path, OpenAPI]] =
    sources.toVector
      .traverse[Result, (Path, OpenAPI)] {
        case (filename, source) =>
          val filepath = Path.of(filename)
          for {
            api <- parse(source, filepath)
          } yield (filepath, api)
      }
      .map(_.toMap)

  def parse(source: String, filepath: Path): Result[OpenAPI] = parse(new StringReader(source), filepath)

  def parse(reader: Reader, filepath: Path): Result[OpenAPI] = {
    val stream = NamedStreamReader(reader, filepath)
    try {
      val parser = new ParserImpl(stream)
      val resolver = new Resolver()
      val composer = new Composer(parser, resolver)
      val node = composer.getSingleNode
      parse(node, filepath)
    } catch {
      case e: MarkedYAMLException => error(e.getProblemMark, e.getMessage)
      case e: YAMLException       => error(stream.getMark, e.getMessage)
    } finally {
      reader.close()
    }
  }

  def parse(node: Node, filepath: Path): Result[OpenAPI] =
    for {
      root <- requireObjectNode(node)
      _ <- root.allowedFields("openapi", "info", "externalDocs", "servers", "security", "tags", "paths", "components")
      openapi <- parseVersion(root)
      info <- parseInfo(root)
      externalDocs <- parseExternalDocs(root)
      servers <- parseServers(root)
      security <- parseSecurityRequirements(root)
      tags <- parseTags(root)
      paths <- parsePaths(root)
      components <- parseComponents(root)
    } yield OpenAPI(
      filepath = filepath,
      node = root.node,
      openapi = openapi,
      info = info,
      externalDocs = externalDocs,
      servers = servers,
      security = security,
      tags = tags,
      paths = paths,
      components = components,
      extensions = root.extensions
    )

  def parseVersion(node: ObjectNode): Result[Scalar[String]] =
    for {
      version <- node.requireScalarField("openapi")
      _ <- require(version.getValue.matches("3\\.0\\.\\d"), version, "unexpected version")
    } yield Scalar(version, version.getValue)

  def parseInfo(root: ObjectNode): Result[ApiInfo] =
    for {
      infoNode <- root.requireObjectField("info")
      _ <- infoNode.allowedFields("title", "description", "termsOfService", "version", "contact", "license")
      title <- infoNode.requireStringConstant("title")
      description <- infoNode.optionalStringConstant("description")
      termsOfService <- infoNode.optionalStringConstant("termsOfService")
      version <- infoNode.requireStringConstant("version")
      contact <- parseContact(infoNode)
      license <- parseLicense(infoNode)
    } yield ApiInfo(
      node = infoNode.node,
      title = title,
      description = description,
      termsOfService = termsOfService,
      contact = contact,
      license = license,
      version = version,
      extensions = infoNode.extensions
    )

  def parseContact(root: ObjectNode): Result[Option[Contact]] =
    root.parseOptionalObject("contact") { contactNode =>
      for {
        _ <- contactNode.allowedFields("name", "url", "email")
        name <- contactNode.optionalStringConstant("name")
        url <- contactNode.optionalStringConstant("url")
        email <- contactNode.optionalStringConstant("email")
      } yield Contact(contactNode.node, name, url, email)
    }

  def parseLicense(root: ObjectNode): Result[Option[License]] =
    root.parseOptionalObject("license") { licenseNode =>
      for {
        _ <- licenseNode.allowedFields("name", "url")
        name <- licenseNode.requireStringConstant("name")
        url <- licenseNode.optionalStringConstant("url")
      } yield License(licenseNode.node, name, url)
    }

  def parseExternalDocs(root: ObjectNode): Result[Option[ExternalDocumentation]] =
    root.parseOptionalObject("externalDocs") { docNode =>
      for {
        _ <- docNode.allowedFields("url", "description")
        url <- docNode.requireStringConstant("url")
        description <- docNode.optionalStringConstant("description")
      } yield ExternalDocumentation(docNode.node, url, description)
    }

  def parseServers(root: ObjectNode): Result[Vector[Server]] =
    root.parseSeq("servers") { node =>
      for {
        serverNode <- requireObjectNode(node)
        server <- parseServer(serverNode)
      } yield server
    }

  def parseServer(serverNode: ObjectNode): Result[Server] =
    for {
      _ <- serverNode.allowedFields("url", "description", "variables")
      url <- serverNode.requireStringConstant("url")
      description <- serverNode.optionalStringConstant("description")
      variables <- parseServerVariables(serverNode)
    } yield Server(serverNode.node, url, description, variables)

  def parseServerVariables(serverNode: ObjectNode): Result[ScalarMap[String, ServerVariable]] =
    serverNode.parseMap("variables") { (_, value) =>
      for {
        variableNode <- requireObjectNode(value)
        variable <- parseServerVariable(variableNode)
      } yield variable
    }

  def parseServerVariable(variableNode: ObjectNode): Result[ServerVariable] =
    for {
      _ <- variableNode.allowedFields("enum", "default", "description")
      default <- variableNode.requireStringConstant("default")
      description <- variableNode.optionalStringConstant("description")
      enum <- variableNode.parseStringSeq("enum")
    } yield ServerVariable(variableNode.node, enum, default, description)

  def parseSecurityRequirements(root: ObjectNode): Result[Vector[SecurityRequirement]] =
    root.parseSeq("security") { node =>
      for {
        requirementNode <- requireObjectNode(node)
        requirement <- parseSecurityRequirement(requirementNode)
      } yield requirement
    }

  def parseSecurityRequirement(requirementNode: ObjectNode): Result[SecurityRequirement] =
    for {
      values <- requirementNode.parseMap { (_, value) =>
        for {
          sequenceNode <- requireSequenceNode(value)
          values <- parseStringSequence(sequenceNode)
        } yield values
      }
    } yield SecurityRequirement(requirementNode.node, values)

  def parseTags(root: ObjectNode): Result[Vector[ApiTag]] =
    root.parseSeq("tags") { node =>
      for {
        tagNode <- requireObjectNode(node)
        tag <- parseTag(tagNode)
      } yield tag
    }

  def parseTag(tagNode: ObjectNode): Result[ApiTag] =
    for {
      _ <- tagNode.allowedFields("name", "description", "externalDocs")
      name <- tagNode.requireStringConstant("name")
      description <- tagNode.optionalStringConstant("description")
      externalDocs <- parseExternalDocs(tagNode)
    } yield ApiTag(tagNode.node, name, description, externalDocs)

  def parsePaths(root: ObjectNode): Result[Paths] =
    for {
      paths <- root.parseMap("paths") { (path, value) =>
        for {
          _ <- require(
            path.value.matches("^(\\/([a-zA-Z0-9_]+|\\{[a-zA-Z0-9_]+\\}))+$"),
            path.node,
            s"illegal path format: ${path.value}"
          )
          itemNode <- requireObjectNode(value)
          item <- parsePathItem(itemNode)
        } yield item
      }
    } yield Paths(paths)

  def parsePathItem(itemNode: ObjectNode): Result[PathItem] =
    for {
      _ <- itemNode.allowedFields(
        "summary",
        "description",
        "get",
        "put",
        "post",
        "delete",
        "options",
        "head",
        "patch",
        "trace",
        "servers",
        "parameters"
      )
      summary <- itemNode.optionalStringConstant("summary")
      description <- itemNode.optionalStringConstant("description")
      get <- itemNode.parseOptionalObject("get")(parseOperation)
      put <- itemNode.parseOptionalObject("put")(parseOperation)
      post <- itemNode.parseOptionalObject("post")(parseOperation)
      delete <- itemNode.parseOptionalObject("delete")(parseOperation)
      options <- itemNode.parseOptionalObject("options")(parseOperation)
      head <- itemNode.parseOptionalObject("head")(parseOperation)
      patch <- itemNode.parseOptionalObject("patch")(parseOperation)
      trace <- itemNode.parseOptionalObject("trace")(parseOperation)
      servers <- parseServers(itemNode)
      parameters <- parseParameters(itemNode)
    } yield PathItem(
      node = itemNode.node,
      summary = summary,
      description = description,
      get = get,
      put = put,
      post = post,
      delete = delete,
      options = options,
      head = head,
      patch = patch,
      trace = trace,
      servers = servers,
      parameters = parameters
    )

  def parseOperation(operationNode: ObjectNode): Result[Operation] =
    for {
      _ <- operationNode.allowedFields(
        "tags",
        "summary",
        "description",
        "externalDocs",
        "operationId",
        "parameters",
        "requestBody",
        "responses",
        "callbacks",
        "deprecated",
        "security"
      )
      tags <- operationNode.parseStringSeq("tags")
      summary <- operationNode.optionalStringConstant("summary")
      description <- operationNode.optionalStringConstant("description")
      externalDocs <- parseExternalDocs(operationNode)
      operationId <- operationNode.optionalStringConstant("operationId")
      parameters <- parseParameters(operationNode)
      requestBody <- operationNode.parseOptionalObject("requestBody")(parseRequestBody)
      responsesNode <- operationNode.requireObjectField("responses")
      responses <- parseResponses(responsesNode)
      deprecated <- operationNode.optionalBooleanConstant("deprecated")
      security <- parseSecurityRequirements(operationNode)
      callbacks <- parseCallbacks(operationNode)
    } yield Operation(
      node = operationNode.node,
      tags = tags,
      summary = summary,
      description = description,
      externalDocs = externalDocs,
      operationId = operationId,
      parameters = parameters,
      requestBody = requestBody,
      responses = responses,
      callbacks = callbacks,
      deprecated = deprecated,
      security = security,
      extensions = operationNode.extensions
    )

  def parseRequestBody(requestNode: ObjectNode): Result[RequestBody] =
    for {
      _ <- requestNode.allowedFields("description", "content", "required")
      description <- requestNode.optionalStringConstant("description")
      contentNode <- requestNode.requireObjectField("content")
      content <- parseContent(contentNode)
      required <- requestNode.optionalBooleanConstant("required")
    } yield RequestBody(requestNode.node, description, content, required)

  def parseResponses(responsesNode: ObjectNode): Result[ApiResponses] =
    for {
      responses <- responsesNode.parseMap { (field, value) =>
        for {
          _ <- require(field.value.matches("^(default|[12345][0-9][0-9])$"), field.node, "illegal http status code")
          responseNode <- requireObjectNode(value)
          response <- parseResponse(responseNode)
        } yield response
      }
    } yield ApiResponses(responsesNode.node, responses)

  def parseResponse(responseNode: ObjectNode): Result[ApiResponse] =
    for {
      _ <- responseNode.allowedFields("description", "headers", "content", "links")
      description <- responseNode.requireStringConstant("description")
      headers <- parseHeaders(responseNode)
      content <- responseNode.parseOptionalObject("content")(parseContent)
      links <- parseLinks(responseNode)
    } yield ApiResponse(responseNode.node, description, headers, content, links, responseNode.extensions)

  def parseHeaders(root: ObjectNode): Result[ScalarMap[String, Header]] =
    root.parseMap("headers") { (headerName, value) =>
      for {
        _ <- require(
          headerName.value.matches("^([a-zA-Z][a-zA-Z0-9]*)(-[a-zA-Z][a-zA-Z0-9]*)*$"),
          headerName.node,
          s"illegal header name: $headerName"
        )
        headerNode <- requireObjectNode(value)
        header <- parseHeader(headerNode)
      } yield header
    }

  def parseHeader(headerNode: ObjectNode): Result[Header] =
    for {
      _ <- headerNode.allowedFields("description", "required", "deprecated")
      description <- headerNode.requireStringConstant("description")
      required <- headerNode.optionalBooleanConstant("required")
      deprecated <- headerNode.optionalBooleanConstant("deprecated")
    } yield Header(headerNode.node, description, required, deprecated)

  def parseLinks(root: ObjectNode): Result[ScalarMap[String, Link]] =
    root.parseMap("links") { (_, value) =>
      for {
        linkNode <- requireObjectNode(value)
        link <- parseLink(linkNode)
      } yield link
    }

  def parseLink(linkNode: ObjectNode): Result[Link] =
    for {
      _ <- linkNode.allowedFields(
        "operationRef",
        "operationId",
        "parameters",
        "requestBody",
        "headers",
        "description",
        "server"
      )
      operationRef <- linkNode.optionalStringConstant("operationRef")
      operationId <- linkNode.optionalStringConstant("operationId")
      parameters <- linkNode.parseMap("parameters")((_, v) => pure(v))
      requestBody <- linkNode.optionalField("requestBody")
      headers <- parseHeaders(linkNode)
      description <- linkNode.optionalStringConstant("description")
      server <- linkNode.parseOptionalObject("server")(parseServer)
    } yield Link(
      linkNode.node,
      operationRef,
      operationId,
      parameters,
      requestBody,
      headers,
      description,
      server,
      linkNode.extensions
    )

  def parseContent(contentNode: ObjectNode): Result[Content] =
    for {
      media <- contentNode.parseMap { (_, value) =>
        for {
          mediaNode <- requireObjectNode(value)
          media <- parseMediaType(mediaNode)
        } yield media
      }
    } yield Content(contentNode.node, media)

  def parseMediaType(contentNode: ObjectNode): Result[MediaType] =
    for {
      _ <- contentNode.allowedFields("schema", "examples", "example", "encoding", "extensions")
      schema <- contentNode.parseOptionalObject("schema")(parseSchema)
      examples <- parseExamples(contentNode)
      example <- contentNode.optionalField("example")
      encoding <- parseEncodingMap(contentNode)
    } yield MediaType(contentNode.node, schema, examples, example, encoding, contentNode.extensions)

  def parseExamples(root: ObjectNode): Result[ScalarMap[String, Example]] =
    root.parseMap("examples") { (_, value) =>
      for {
        exampleNode <- requireObjectNode(value)
        example <- parseExample(exampleNode)
      } yield example
    }

  def parseExample(exampleNode: ObjectNode): Result[Example] =
    for {
      _ <- exampleNode.allowedFields("summary", "description", "value", "externalValue", "$ref")
      summary <- exampleNode.optionalStringConstant("summary")
      description <- exampleNode.optionalStringConstant("description")
      value <- exampleNode.optionalField("value")
      externalValue <- exampleNode.optionalStringConstant("externalValue")
      ref <- exampleNode.optionalStringConstant("$ref")
      $ref <- parseOptionalRef(ref)
    } yield Example(exampleNode.node, summary, description, value, externalValue, $ref, exampleNode.extensions)

  def parseParameters(root: ObjectNode): Result[Vector[Parameter]] =
    root.parseSeq("parameters") { node =>
      for {
        parameterNode <- requireObjectNode(node)
        parameter <- parseParameter(parameterNode)
      } yield parameter
    }

  def parseParameter(parameterNode: ObjectNode): Result[Parameter] =
    for {
      name <- parameterNode.requireStringConstant("name")
      in <- parameterNode.requireStringConstant("in")
      description <- parameterNode.optionalStringConstant("description")
      required <- parameterNode.optionalBooleanConstant("required")
      deprecated <- parameterNode.optionalBooleanConstant("deprecated")
      allowEmptyValue <- parameterNode.optionalBooleanConstant("allowEmptyValue")
      explode <- parameterNode.optionalBooleanConstant("explode")
      allowReserved <- parameterNode.optionalBooleanConstant("allowReserved")
      schema <- parameterNode.parseOptionalObject("schema")(parseSchema)
      examples <- parseExamples(parameterNode)
      example <- parameterNode.optionalField("example")
      content <- parameterNode.parseOptionalObject("content")(parseContent)
      style <- parseParameterStyle(parameterNode)
    } yield Parameter(
      parameterNode.node,
      name,
      in,
      description,
      required,
      deprecated,
      allowEmptyValue,
      style,
      explode,
      allowReserved,
      schema,
      examples,
      example,
      content,
      parameterNode.extensions
    )

  def parseParameterStyle(parameterNode: ObjectNode): Result[Option[ParameterStyle]] =
    (for {
      style <- OptionT(parameterNode.optionalStringConstant("style"))
      value: ParameterStyle <- OptionT.liftF(style.value match {
        case ParameterStyle.MATRIX.id          => pure(ParameterStyle.MATRIX)
        case ParameterStyle.LABEL.id           => pure(ParameterStyle.LABEL)
        case ParameterStyle.FORM.id            => pure(ParameterStyle.FORM)
        case ParameterStyle.SIMPLE.id          => pure(ParameterStyle.SIMPLE)
        case ParameterStyle.SPACE_DELIMITED.id => pure(ParameterStyle.SPACE_DELIMITED)
        case ParameterStyle.PIPE_DELIMITED.id  => pure(ParameterStyle.PIPE_DELIMITED)
        case ParameterStyle.DEEP_OBJECT.id     => pure(ParameterStyle.DEEP_OBJECT)
        case value                             => error[ParameterStyle](style.node, s"unexpected value: $value")
      })
    } yield value).value

  def parseEncodingMap(root: ObjectNode): Result[ScalarMap[String, Encoding]] =
    root.parseMap("encoding") { (_, value) =>
      for {
        encodingNode <- requireObjectNode(value)
        encoding <- parseEncoding(encodingNode)
      } yield encoding
    }

  def parseEncoding(encodingNode: ObjectNode): Result[Encoding] =
    for {
      _ <- encodingNode.allowedFields("contentType", "headers", "style", "explode", "allowReserved")
      contentType <- encodingNode.optionalStringConstant("contentType")
      headers <- parseHeaders(encodingNode)
      style <- parseParameterStyle(encodingNode)
      explode <- encodingNode.optionalBooleanConstant("explode")
      allowReserved <- encodingNode.optionalBooleanConstant("allowReserved")
    } yield Encoding(encodingNode.node, contentType, headers, style, explode, allowReserved, encodingNode.extensions)

  def parseCallbacks(root: ObjectNode): Result[ScalarMap[String, Callback]] =
    root.parseMap("callbacks") { (_, value) =>
      for {
        callbackNode <- requireObjectNode(value)
        callback <- parseCallback(callbackNode)
      } yield callback
    }

  def parseCallback(callbackNode: ObjectNode): Result[Callback] =
    for {
      values <- callbackNode.parseMap { (_, value) =>
        for {
          itemNode <- requireObjectNode(value)
          item <- parsePathItem(itemNode)
        } yield item
      }
    } yield Callback(callbackNode.node, values)

  def parseSchema(schemaNode: ObjectNode): Result[Schema] =
    if (schemaNode.fields.contains("$ref")) {
      parseSchemaRef(schemaNode)
    } else {
      parseSchemaNormal(schemaNode)
    }

  def parseSchemaRef(schemaNode: ObjectNode): Result[Schema] =
    for {
      _ <- schemaNode.allowedFields("$ref")
      ref <- schemaNode.requireStringConstant("$ref")
      $ref <- parseRef(ref)
    } yield Reference(schemaNode.node, $ref, schemaNode.extensions)

  def parseOptionalRef(opt: Option[Scalar[String]]): Result[Option[SchemaReference]] =
    (for {
      scalar <- OptionT.fromOption[Result](opt)
      $ref <- OptionT.liftF(parseRef(scalar))
    } yield $ref).value

  def parseRef($ref: Scalar[String]): Result[SchemaReference] =
    $ref.value match {
      case schemaRef(id) => pure(ComponentsReference($ref.map(_ => id)))
      case externalRef(file, _, id) =>
        pure(ExternalReference($ref.map(_ => file), ComponentsReference($ref.map(_ => id))))
      case _ => error($ref.node, s"invalid ref: ${$ref.value}")
    }

  def parseSchemaNormal(schemaNode: ObjectNode): Result[Schema] =
    for {
      _ <- schemaNode.allowedFields(
        "type",
        "format",
        "default",
        "name",
        "title",
        "multipleOf",
        "maximum",
        "exclusiveMaximum",
        "minimum",
        "exclusiveMinimum",
        "maxLength",
        "minLength",
        "pattern",
        "maxItems",
        "minItems",
        "uniqueItems",
        "items",
        "allOf",
        "anyOf",
        "oneOf",
        "maxProperties",
        "minProperties",
        "required",
        "not",
        "properties",
        "additionalProperties",
        "description",
        "nullable",
        "readOnly",
        "writeOnly",
        "example",
        "externalDocs",
        "deprecated",
        "xml",
        "enum",
        "discriminator"
      )
      schemaType <- (for {
        typeNode <- OptionT(schemaNode.optionalStringConstant("type"))
        value: SchemaType <- OptionT.liftF(typeNode.value match {
          case SchemaType.INTEGER_TYPE.id => pure(SchemaType.INTEGER_TYPE)
          case SchemaType.NUMBER_TYPE.id  => pure(SchemaType.NUMBER_TYPE)
          case SchemaType.STRING_TYPE.id  => pure(SchemaType.STRING_TYPE)
          case SchemaType.BOOLEAN_TYPE.id => pure(SchemaType.BOOLEAN_TYPE)
          case SchemaType.OBJECT_TYPE.id  => pure(SchemaType.OBJECT_TYPE)
          case SchemaType.ARRAY_TYPE.id   => pure(SchemaType.ARRAY_TYPE)
          case value                      => error[SchemaType](typeNode.node, s"unexpected type: $value")
        })
      } yield value).value
      format <- (for {
        format <- OptionT(schemaNode.optionalStringConstant("format"))
        value: FormatType <- OptionT.liftF(format.value match {
          case FormatType.INTEGER32_FORMAT.id => pure(FormatType.INTEGER32_FORMAT)
          case FormatType.INTEGER64_FORMAT.id => pure(FormatType.INTEGER64_FORMAT)
          case FormatType.FLOAT_FORMAT.id     => pure(FormatType.FLOAT_FORMAT)
          case FormatType.DOUBLE_FORMAT.id    => pure(FormatType.DOUBLE_FORMAT)
          case FormatType.BYTE_FORMAT.id      => pure(FormatType.BYTE_FORMAT)
          case FormatType.BINARY_FORMAT.id    => pure(FormatType.BINARY_FORMAT)
          case FormatType.DATE_FORMAT.id      => pure(FormatType.DATE_FORMAT)
          case FormatType.DATE_TIME_FORMAT.id => pure(FormatType.DATE_TIME_FORMAT)
          case FormatType.PASSWORD_FORMAT.id  => pure(FormatType.PASSWORD_FORMAT)
          case FormatType.EMAIL_FORMAT.id     => pure(FormatType.EMAIL_FORMAT)
          case FormatType.UUID_FORMAT.id      => pure(FormatType.UUID_FORMAT)
          case FormatType.URI_FORMAT.id       => pure(FormatType.URI_FORMAT)
          case value                          => error[FormatType](format.node, s"unexpected format: $value")
        })
      } yield value).value
      default <- schemaNode.optionalField("default")
      name <- schemaNode.optionalStringConstant("name")
      title <- schemaNode.optionalStringConstant("title")
      multipleOf <- schemaNode.optionalBigDecimalConstant("multipleOf")
      maximum <- schemaNode.optionalBigDecimalConstant("maximum")
      exclusiveMaximum <- schemaNode.optionalBooleanConstant("exclusiveMaximum")
      minimum <- schemaNode.optionalBigDecimalConstant("minimum")
      exclusiveMinimum <- schemaNode.optionalBooleanConstant("exclusiveMinimum")
      maxLength <- schemaNode.optionalIntConstant("maxLength")
      minLength <- schemaNode.optionalIntConstant("minLength")
      pattern <- schemaNode.optionalStringConstant("pattern")
      maxItems <- schemaNode.optionalIntConstant("maxItems")
      minItems <- schemaNode.optionalIntConstant("minItems")
      uniqueItems <- schemaNode.optionalBooleanConstant("uniqueItems")
      items <- schemaNode.parseOptionalObject("items")(parseSchema)
      allOf <- parseSchemas(schemaNode, "allOf")
      anyOf <- parseSchemas(schemaNode, "anyOf")
      oneOf <- parseSchemas(schemaNode, "oneOf")
      maxProperties <- schemaNode.optionalIntConstant("maxProperties")
      minProperties <- schemaNode.optionalIntConstant("minProperties")
      required <- schemaNode.parseStringSeq("required")
      not <- schemaNode.parseOptionalObject("not")(parseSchema)
      properties <- schemaNode.parseMap("properties") { (_, value) =>
        for {
          node <- requireObjectNode(value)
          schema <- parseSchema(node)
        } yield schema
      }
      additionalProperties <- schemaNode.optionalField("additionalProperties")
      description <- schemaNode.optionalStringConstant("nullable")
      nullable <- schemaNode.optionalBooleanConstant("nullable")
      readOnly <- schemaNode.optionalBooleanConstant("readOnly")
      writeOnly <- schemaNode.optionalBooleanConstant("writeOnly")
      example <- schemaNode.optionalField("example")
      externalDocs <- parseExternalDocs(schemaNode)
      deprecated <- schemaNode.optionalBooleanConstant("deprecated")
      xml <- schemaNode.optionalField("xml")
      enum <- schemaNode.parseStringSeq("enum")
      discriminator <- schemaNode.parseOptionalObject("discriminator")(parseDiscriminator)
    } yield SchemaNormal(
      node = schemaNode.node,
      `type` = schemaType,
      format = format,
      default = default,
      name = name,
      title = title,
      multipleOf = multipleOf,
      maximum = maximum,
      exclusiveMaximum = exclusiveMaximum,
      minimum = minimum,
      exclusiveMinimum = exclusiveMinimum,
      maxLength = maxLength,
      minLength = minLength,
      pattern = pattern,
      maxItems = maxItems,
      minItems = minItems,
      uniqueItems = uniqueItems,
      items = items,
      allOf = allOf,
      anyOf = anyOf,
      oneOf = oneOf,
      maxProperties = maxProperties,
      minProperties = minProperties,
      required = required,
      not = not,
      properties = properties,
      additionalProperties = additionalProperties,
      description = description,
      nullable = nullable,
      readOnly = readOnly,
      writeOnly = writeOnly,
      example = example,
      externalDocs = externalDocs,
      deprecated = deprecated,
      xml = xml,
      enum = enum,
      discriminator = discriminator,
      extensions = schemaNode.extensions
    )

  def parseDiscriminator(discriminatorNode: ObjectNode): Result[Discriminator] =
    for {
      propertyName <- discriminatorNode.requireStringConstant("propertyName")
      mapping <- discriminatorNode.parseMap("mapping") { (_, value) =>
        for {
          scalar <- requireScalarNode(value)
        } yield Scalar(scalar, scalar.getValue)
      }
    } yield Discriminator(discriminatorNode.node, propertyName, mapping)

  def parseComponents(root: ObjectNode): Result[Components] =
    for {
      componentsNode <- root.requireObjectField("components")
      _ <- componentsNode.allowedFields(
        "schemas",
        "parameters",
        "examples",
        "headers",
        "securitySchemes",
        "links",
        "callbacks"
      )
      schemas <- componentsNode.parseMap("schemas") { (_, value) =>
        for {
          schemaNode <- requireObjectNode(value)
          schema <- parseSchema(schemaNode)
        } yield schema
      }
      parameters <- componentsNode.parseMap("parameters") { (_, value) =>
        for {
          parameterNode <- requireObjectNode(value)
          parameter <- parseParameter(parameterNode)
        } yield parameter
      }
      examples <- parseExamples(componentsNode)
      headers <- parseHeaders(componentsNode)
      securitySchemes <- componentsNode.parseMap("securitySchemes") { (_, value) =>
        for {
          securityNode <- requireObjectNode(value)
          securityScheme <- parseSecurityScheme(securityNode)
        } yield securityScheme
      }
      links <- parseLinks(componentsNode)
      callbacks <- parseCallbacks(componentsNode)
    } yield Components(
      node = componentsNode.node,
      schemas = schemas,
      parameters = parameters,
      examples = examples,
      headers = headers,
      securitySchemes = securitySchemes,
      links = links,
      callbacks = callbacks,
      extensions = componentsNode.extensions
    )

  def parseSecurityScheme(securityNode: ObjectNode): Result[SecurityScheme] =
    for {
      _ <- securityNode.allowedFields(
        "type",
        "description",
        "name",
        "in",
        "scheme",
        "bearerFormat",
        "flows",
        "openIdConnectUrl"
      )
      securityType <- (for {
        constant <- OptionT(securityNode.optionalStringConstant("type"))
        value: SecurityScheme.Type <- OptionT.liftF(constant.value match {
          case SecurityScheme.API_KEY.id         => pure(SecurityScheme.API_KEY)
          case SecurityScheme.HTTP.id            => pure(SecurityScheme.HTTP)
          case SecurityScheme.OAUTH2.id          => pure(SecurityScheme.OAUTH2)
          case SecurityScheme.OPEN_ID_CONNECT.id => pure(SecurityScheme.OPEN_ID_CONNECT)
          case value                             => error[SecurityScheme.Type](constant.node, s"unexpected type: $value")
        })
      } yield value).value
      description <- securityNode.optionalStringConstant("description")
      name <- securityNode.optionalStringConstant("name")
      in <- (for {
        constant <- OptionT(securityNode.optionalStringConstant("in"))
        value: SecurityScheme.In <- OptionT.liftF(constant.value match {
          case SecurityScheme.COOKIE.id => pure(SecurityScheme.COOKIE)
          case SecurityScheme.HEADER.id => pure(SecurityScheme.HEADER)
          case SecurityScheme.QUERY.id  => pure(SecurityScheme.QUERY)
          case value                    => error[SecurityScheme.In](constant.node, s"unexpected type: $value")
        })
      } yield value).value
      scheme <- securityNode.optionalStringConstant("scheme")
      bearerFormat <- securityNode.optionalStringConstant("bearerFormat")
      flows <- securityNode.parseOptionalObject("flows")(parseOAuthFlows)
      openIdConnectUrl <- securityNode.optionalStringConstant("openIdConnectUrl")
    } yield SecurityScheme(
      node = securityNode.node,
      `type` = securityType,
      description = description,
      name = name,
      in = in,
      scheme = scheme,
      bearerFormat = bearerFormat,
      flows = flows,
      openIdConnectUrl = openIdConnectUrl,
      extensions = securityNode.extensions
    )

  def parseOAuthFlows(flowsNode: ObjectNode): Result[OAuthFlows] =
    for {
      _ <- flowsNode.allowedFields("implicit", "password", "clientCredentials", "authorizationCode")
      implicitly <- flowsNode.parseOptionalObject("implicit")(parseOAuthFlow)
      password <- flowsNode.parseOptionalObject("password")(parseOAuthFlow)
      clientCredentials <- flowsNode.parseOptionalObject("clientCredentials")(parseOAuthFlow)
      authorizationCode <- flowsNode.parseOptionalObject("authorizationCode")(parseOAuthFlow)
    } yield OAuthFlows(
      node = flowsNode.node,
      `implicit` = implicitly,
      password = password,
      clientCredentials = clientCredentials,
      authorizationCode = authorizationCode
    )

  def parseOAuthFlow(flowNode: ObjectNode): Result[OAuthFlow] =
    for {
      _ <- flowNode.allowedFields("authorizationUrl", "tokenUrl", "refreshUrl", "scopes")
      authorizationUrl <- flowNode.optionalStringConstant("authorizationUrl")
      tokenUrl <- flowNode.optionalStringConstant("tokenUrl")
      refreshUrl <- flowNode.optionalStringConstant("refreshUrl")
      scopes <- flowNode.parseOptionalObject("scopes")(parseSecurityScopes)
    } yield OAuthFlow(
      node = flowNode.node,
      authorizationUrl = authorizationUrl,
      tokenUrl = tokenUrl,
      refreshUrl = refreshUrl,
      scopes = scopes,
      extensions = flowNode.extensions
    )

  def parseSecurityScopes(scopesNode: ObjectNode): Result[SecurityScopes] =
    for {
      values <- scopesNode.parseMap { (_, value) =>
        for {
          scalar <- requireScalarNode(value)
        } yield Scalar(scalar, scalar.getValue)
      }
    } yield SecurityScopes(node = scopesNode.node, values = values, extensions = scopesNode.extensions)

  /* complex */

  def parseSchemas(root: ObjectNode, fieldName: String): Result[Vector[Schema]] =
    root.parseSeq(fieldName) { node =>
      for {
        schemaNode <- requireObjectNode(node)
        schema <- parseSchema(schemaNode)
      } yield schema
    }

  def parseStringSequence(node: SequenceNode): Result[Vector[Scalar[String]]] =
    node.getValue.asScala.toVector.traverse[Result, Scalar[String]] { node =>
      for {
        scalar <- requireScalarNode(node)
      } yield Scalar(scalar, scalar.getValue)
    }

  /* primitives */

  def requireObjectNode(node: Node): Result[ObjectNode] =
    for {
      mappingNode <- node match {
        case s: MappingNode => pure(s)
        case _              => error(node, s"expected map, actual ${node.getTag}")
      }
      fields <- mappingNode.getValue.asScala.toVector
        .traverse[Result, (Scalar[String], Node)] { tuple =>
          for {
            scalar <- requireScalarNode(tuple.getKeyNode)
          } yield (Scalar(scalar, scalar.getValue), tuple.getValueNode)
        }
      _ <- requireUnique(node, fields.map(_._1), "keys not unique")
    } yield ObjectNode(mappingNode, ScalarMap(fields))

  def requireAnchorNode(node: Node): Result[AnchorNode] =
    node match {
      case s: AnchorNode => pure(s)
      case _             => error(node, s"expected anchor, actual ${node.getTag}")
    }

  def requireScalarNode(node: Node): Result[ScalarNode] =
    node match {
      case s: ScalarNode => pure(s)
      case _             => error(node, s"expected scalar, actual ${node.getTag}")
    }

  def requireSequenceNode(node: Node): Result[SequenceNode] =
    node match {
      case s: SequenceNode => pure(s)
      case _               => error(node, s"expected sequence, actual ${node.getTag}")
    }

  def requireUnique[A](node: Node, seq: Iterable[A], message: => String): Result[Unit] = {
    val set = scala.collection.mutable.Set[A]()
    val unique = seq.forall { x =>
      if (set(x)) false
      else {
        set += x
        true
      }
    }
    require(unique, node, message)
  }
}
