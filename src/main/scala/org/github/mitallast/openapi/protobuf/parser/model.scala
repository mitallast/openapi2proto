package org.github.mitallast.openapi.protobuf.parser

import java.util.Objects

import cats.Show
import cats.implicits._
import org.yaml.snakeyaml.error.Mark
import org.yaml.snakeyaml.nodes.{MappingNode, Node, NodeTuple, ScalarNode}

import scala.collection.MapView

object implicits {
  import cats.derived, derived.auto.show._

  implicit def showScalar[V](implicit V: Show[V]): Show[Scalar[V]] = Show.show(_.value.show)

  implicit val showMark: Show[Mark] = Show.show(m => s"${m.getName}:${m.getLine}:${m.getColumn}")
  implicit val showNode: Show[Node] = Show.show(_.getStartMark.show)
  implicit val showScalarNode: Show[ScalarNode] = Show.show(_.getStartMark.show)
  implicit val showMappingNode: Show[MappingNode] = Show.show(_.getStartMark.show)
  implicit val showNodeTuple: Show[NodeTuple] = Show.show(_.getKeyNode.show)

  implicit def showScalarMap[K, V](implicit K: Show[K], V: Show[V]): Show[ScalarMap[K, V]] = Show.show { scalarMap =>
    scalarMap.view
      .map {
        case (k, v) =>
          show"$k -> $v"
      }
      .mkString("{", ", ", "}")
  }

  implicit val showSchemaType: Show[SchemaType] = Show.show(_.id)
  implicit val showFormatType: Show[FormatType] = Show.show(_.id)

  implicit val showSchema: Show[Schema] = Show.show {
    case r: Reference    => r.show
    case s: SchemaNormal => s.show
  }
  implicit val showParameterStyle: Show[ParameterStyle] = Show.show(_.id)
  implicit val showSecuritySchemeType: Show[SecurityScheme.Type] = Show.show(_.id)
  implicit val showSecuritySchemeIn: Show[SecurityScheme.In] = Show.show(_.id)

  implicit val showSecurityScopes: Show[SecurityScopes] = derived.semi.show[SecurityScopes]
  implicit val showOAuthFlow: Show[OAuthFlow] = derived.semi.show[OAuthFlow]
  implicit val showOAuthFlows: Show[OAuthFlows] = derived.semi.show[OAuthFlows]

  implicit val showSecurityRequirement: Show[SecurityRequirement] = derived.semi.show[SecurityRequirement]
  implicit val showApiInfo: Show[ApiInfo] = derived.semi.show[ApiInfo]
  implicit val showExternalDocumentation: Show[ExternalDocumentation] = derived.semi.show[ExternalDocumentation]
  implicit val showContact: Show[Contact] = derived.semi.show[Contact]
  implicit val showLicense: Show[License] = derived.semi.show[License]
  implicit val showServer: Show[Server] = derived.semi.show[Server]
  implicit val showServerVariable: Show[ServerVariable] = derived.semi.show[ServerVariable]
  implicit val showApiTag: Show[ApiTag] = derived.semi.show[ApiTag]

  implicit val showPathItem: Show[PathItem] = derived.semi.show[PathItem]
  implicit val showOperation: Show[Operation] = derived.semi.show[Operation]
  implicit val showRequestBody: Show[RequestBody] = derived.semi.show[RequestBody]
  implicit val showContent: Show[Content] = derived.semi.show[Content]
  implicit val showMediaType: Show[MediaType] = derived.semi.show[MediaType]
  implicit val showExample: Show[Example] = derived.semi.show[Example]
  implicit val showEncoding: Show[Encoding] = derived.semi.show[Encoding]
  implicit val showHeader: Show[Header] = derived.semi.show[Header]

  implicit val showReference: Show[Reference] = derived.semi.show[Reference]
  implicit val showSchemaNormal: Show[SchemaNormal] = derived.semi.show[SchemaNormal]
  implicit val showDiscriminator: Show[Discriminator] = derived.semi.show[Discriminator]
  implicit val showApiResponses: Show[ApiResponses] = derived.semi.show[ApiResponses]
  implicit val showApiResponse: Show[ApiResponse] = derived.semi.show[ApiResponse]
  implicit val showLink: Show[Link] = derived.semi.show[Link]
  implicit val showCallback: Show[Callback] = derived.semi.show[Callback]
  implicit val showParameter: Show[Parameter] = derived.semi.show[Parameter]

  implicit val showPaths: Show[Paths] = derived.semi.show[Paths]
  implicit val showComponents: Show[Components] = derived.semi.show[Components]
  implicit val showOpenAPI: Show[OpenAPI] = derived.semi.show[OpenAPI]
}

object Scalar {
  def apply(node: Node, value: Int): Scalar[Int] = new Scalar(node, value)
  def apply(node: Node, value: String): Scalar[String] = new Scalar(node, value)
  def apply(node: Node, value: Boolean): Scalar[Boolean] = new Scalar(node, value)
  def apply(node: Node, value: BigDecimal): Scalar[BigDecimal] = new Scalar(node, value)
}
final class Scalar[V](val node: Node, val value: V) {
  def map[VV](f: V => VV): Scalar[VV] = new Scalar(node, f(value))

  override def hashCode(): Int = Objects.hash(value)
  override def equals(obj: Any): Boolean = Objects.equals(obj, this)

  override def toString: String = value.toString
}

object ScalarMap {
  def empty[K, V]: ScalarMap[K, V] = apply(Vector.empty)
  def apply[K, V](values: Vector[(Scalar[K], V)]): ScalarMap[K, V] = new ScalarMap[K, V](values)
}
final class ScalarMap[K, V](private val values: Vector[(Scalar[K], V)]) {
  private val map: Map[K, (Scalar[K], V)] = values.map { case (k, v) => (k.value, (k, v)) }.toMap

  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty
  def size: Int = map.size
  def entries: Vector[(Scalar[K], V)] = values

  def contains(key: K): Boolean = map.contains(key)
  def contains(key: Scalar[K]): Boolean = map.contains(key.value)

  def get(key: K): Option[V] = map.get(key).map(_._2)
  def get(key: Scalar[K]): Option[V] = map.get(key.value).map(_._2)

  def apply(key: K): V = map(key)._2
  def apply(key: Scalar[K]): V = map(key.value)._2

  def filter(pred: ((Scalar[K], V)) => Boolean): ScalarMap[K, V] = ScalarMap(values.filter(pred))
  def filterNot(pred: ((Scalar[K], V)) => Boolean): ScalarMap[K, V] = ScalarMap(values.filterNot(pred))

  def view: MapView[K, V] = map.view.mapValues(_._2)
  def toVector: Vector[(Scalar[K], V)] = entries
}

final case class OpenAPI(
  node: MappingNode,
  openapi: Scalar[String],
  info: ApiInfo,
  externalDocs: Option[ExternalDocumentation],
  servers: Vector[Server],
  security: Vector[SecurityRequirement],
  tags: Vector[ApiTag],
  paths: Paths,
  components: Components,
  extensions: ScalarMap[String, Node]
) {
  import implicits._

  override def toString: String = this.show
}

final case class SecurityRequirement(node: MappingNode, values: ScalarMap[String, Vector[Scalar[String]]])

final case class ApiInfo(
  node: MappingNode,
  title: Scalar[String],
  description: Option[Scalar[String]],
  termsOfService: Option[Scalar[String]],
  contact: Option[Contact],
  license: Option[License],
  version: Scalar[String],
  extensions: ScalarMap[String, Node]
)

final case class ExternalDocumentation(node: MappingNode, url: Scalar[String], description: Option[Scalar[String]])

final case class Contact(
  node: MappingNode,
  name: Option[Scalar[String]],
  url: Option[Scalar[String]],
  email: Option[Scalar[String]]
)

final case class License(node: MappingNode, name: Scalar[String], url: Option[Scalar[String]])

final case class Server(
  node: MappingNode,
  url: Scalar[String],
  description: Option[Scalar[String]],
  variables: ScalarMap[String, ServerVariable]
)

final case class ServerVariable(
  node: MappingNode,
  enum: Vector[Scalar[String]],
  default: Scalar[String],
  description: Option[Scalar[String]]
)

final case class ApiTag(
  node: MappingNode,
  name: Scalar[String],
  description: Option[Scalar[String]],
  externalDocs: Option[ExternalDocumentation]
)

final case class Paths(node: MappingNode, values: ScalarMap[String, PathItem])

sealed trait HttpMethod
object HttpMethod {
  case object get extends HttpMethod
  case object put extends HttpMethod
  case object post extends HttpMethod
  case object delete extends HttpMethod
  case object options extends HttpMethod
  case object head extends HttpMethod
  case object patch extends HttpMethod
  case object trace extends HttpMethod
}

final case class PathItem(
  node: MappingNode,
  summary: Option[Scalar[String]],
  description: Option[Scalar[String]],
  get: Option[Operation],
  put: Option[Operation],
  post: Option[Operation],
  delete: Option[Operation],
  options: Option[Operation],
  head: Option[Operation],
  patch: Option[Operation],
  trace: Option[Operation],
  servers: Vector[Server],
  parameters: Vector[Parameter]
) {
  def operations: Vector[(HttpMethod, Operation)] =
    Vector(
      (HttpMethod.get, get),
      (HttpMethod.put, put),
      (HttpMethod.post, post),
      (HttpMethod.delete, delete),
      (HttpMethod.options, options),
      (HttpMethod.head, head),
      (HttpMethod.patch, patch),
      (HttpMethod.trace, trace)
    ).collect {
      case (m, Some(op)) => (m, op)
    }
}

final case class Operation(
  node: Node,
  tags: Vector[Scalar[String]],
  summary: Option[Scalar[String]],
  description: Option[Scalar[String]],
  externalDocs: Option[ExternalDocumentation],
  operationId: Option[Scalar[String]],
  parameters: Vector[Parameter],
  requestBody: Option[RequestBody],
  responses: ApiResponses,
  callbacks: ScalarMap[String, Callback],
  deprecated: Option[Scalar[Boolean]],
  security: Vector[SecurityRequirement]
)

final case class RequestBody(
  node: MappingNode,
  description: Option[Scalar[String]],
  content: Content,
  required: Option[Scalar[Boolean]]
)

final case class Content(node: Node, media: ScalarMap[String, MediaType])

final case class MediaType(
  node: MappingNode,
  schema: Option[Schema],
  examples: ScalarMap[String, Example],
  example: Option[Node],
  encoding: ScalarMap[String, Encoding],
  extensions: ScalarMap[String, Node]
)

final case class Example(
  node: MappingNode,
  summary: Option[Scalar[String]],
  description: Option[Scalar[String]],
  value: Option[Node],
  externalValue: Option[Scalar[String]],
  $ref: Option[Scalar[String]],
  extensions: ScalarMap[String, Node]
)

final case class Encoding(
  node: MappingNode,
  contentType: Option[Scalar[String]],
  headers: ScalarMap[String, Header],
  style: Option[ParameterStyle],
  explode: Option[Scalar[Boolean]],
  allowReserved: Option[Scalar[Boolean]],
  extensions: ScalarMap[String, Node]
)

final case class Header(
  node: MappingNode,
  description: Scalar[String],
  required: Option[Scalar[Boolean]],
  deprecated: Option[Scalar[Boolean]]
)

sealed trait SchemaType {
  def id: String
}
object SchemaType {
  case object INTEGER_TYPE extends SchemaType { override val id: String = "integer" }
  case object NUMBER_TYPE extends SchemaType { override val id: String = "number" }
  case object STRING_TYPE extends SchemaType { override val id: String = "string" }
  case object BOOLEAN_TYPE extends SchemaType { override val id: String = "boolean" }
  case object OBJECT_TYPE extends SchemaType { override val id: String = "object" }
  case object ARRAY_TYPE extends SchemaType { override val id: String = "array" }
}
sealed trait FormatType {
  def id: String
}
object FormatType {
  case object INTEGER32_FORMAT extends FormatType { override val id: String = "int32" }
  case object INTEGER64_FORMAT extends FormatType { override val id: String = "int64" }
  case object FLOAT_FORMAT extends FormatType { override val id: String = "float" }
  case object DOUBLE_FORMAT extends FormatType { override val id: String = "double" }
  case object BYTE_FORMAT extends FormatType { override val id: String = "byte" }
  case object BINARY_FORMAT extends FormatType { override val id: String = "binary" }
  case object DATE_FORMAT extends FormatType { override val id: String = "date" }
  case object DATE_TIME_FORMAT extends FormatType { override val id: String = "date-time" }
  case object PASSWORD_FORMAT extends FormatType { override val id: String = "password" }
  case object EMAIL_FORMAT extends FormatType { override val id: String = "email" }
  case object UUID_FORMAT extends FormatType { override val id: String = "uuid" }
  case object URI_FORMAT extends FormatType { override val id: String = "uri" }
}

sealed trait Schema {
  def node: MappingNode
  def extensions: ScalarMap[String, Node]
}
final case class Reference(node: MappingNode, $ref: Scalar[String], extensions: ScalarMap[String, Node]) extends Schema
final case class SchemaNormal(
  node: MappingNode,
  `type`: Option[SchemaType],
  format: Option[FormatType],
  default: Option[Node],
  name: Option[Scalar[String]],
  title: Option[Scalar[String]],
  multipleOf: Option[Scalar[BigDecimal]],
  maximum: Option[Scalar[BigDecimal]],
  exclusiveMaximum: Option[Scalar[Boolean]],
  minimum: Option[Scalar[BigDecimal]],
  exclusiveMinimum: Option[Scalar[Boolean]],
  maxLength: Option[Scalar[Int]],
  minLength: Option[Scalar[Int]],
  pattern: Option[Scalar[String]],
  maxItems: Option[Scalar[Int]],
  minItems: Option[Scalar[Int]],
  uniqueItems: Option[Scalar[Boolean]],
  items: Option[Schema],
  allOf: Vector[Schema],
  anyOf: Vector[Schema],
  oneOf: Vector[Schema],
  maxProperties: Option[Scalar[Int]],
  minProperties: Option[Scalar[Int]],
  required: Vector[Scalar[String]],
  not: Option[Schema],
  properties: ScalarMap[String, Schema],
  additionalProperties: Option[Node],
  description: Option[Scalar[String]],
  nullable: Option[Scalar[Boolean]],
  readOnly: Option[Scalar[Boolean]],
  writeOnly: Option[Scalar[Boolean]],
  example: Option[Node],
  externalDocs: Option[ExternalDocumentation],
  deprecated: Option[Scalar[Boolean]],
  xml: Option[Node],
  enum: Vector[Scalar[String]],
  discriminator: Option[Discriminator],
  extensions: ScalarMap[String, Node]
) extends Schema

final case class Discriminator(
  node: MappingNode,
  propertyName: Scalar[String],
  mapping: ScalarMap[String, Scalar[String]]
)

final case class ApiResponses(node: MappingNode, values: ScalarMap[String, ApiResponse])

final case class ApiResponse(
  node: MappingNode,
  description: Scalar[String],
  headers: ScalarMap[String, Header],
  content: Option[Content],
  links: ScalarMap[String, Link],
  extensions: ScalarMap[String, Node]
)

final case class Link(
  node: MappingNode,
  operationRef: Option[Scalar[String]],
  operationId: Option[Scalar[String]],
  parameters: ScalarMap[String, Node],
  requestBody: Option[Node],
  headers: ScalarMap[String, Header],
  description: Option[Scalar[String]],
  server: Option[Server],
  extensions: ScalarMap[String, Node]
)

final case class Callback(node: MappingNode, values: ScalarMap[String, PathItem])

sealed trait ParameterStyle {
  def id: String
}
object ParameterStyle {
  case object MATRIX extends ParameterStyle { override val id: String = "matrix" }
  case object LABEL extends ParameterStyle { override val id: String = "label" }
  case object FORM extends ParameterStyle { override val id: String = "form" }
  case object SIMPLE extends ParameterStyle { override val id: String = "simple" }
  case object SPACE_DELIMITED extends ParameterStyle { override val id: String = "spaceDelimited" }
  case object PIPE_DELIMITED extends ParameterStyle { override val id: String = "pipeDelimited" }
  case object DEEP_OBJECT extends ParameterStyle { override val id: String = "deepObject" }
}

final case class Parameter(
  node: MappingNode,
  name: Scalar[String],
  in: Scalar[String],
  description: Option[Scalar[String]],
  required: Option[Scalar[Boolean]],
  deprecated: Option[Scalar[Boolean]],
  allowEmptyValue: Option[Scalar[Boolean]],
  style: Option[ParameterStyle],
  explode: Option[Scalar[Boolean]],
  allowReserved: Option[Scalar[Boolean]],
  schema: Option[Schema],
  examples: ScalarMap[String, Example],
  example: Option[Node],
  content: Option[Content],
  extensions: ScalarMap[String, Node]
)

final case class Components(
  node: MappingNode,
  schemas: ScalarMap[String, Schema],
  parameters: ScalarMap[String, Parameter],
  examples: ScalarMap[String, Example],
  headers: ScalarMap[String, Header],
  securitySchemes: ScalarMap[String, SecurityScheme],
  links: ScalarMap[String, Link],
  callbacks: ScalarMap[String, Callback],
  extensions: ScalarMap[String, Node]
)

object SecurityScheme {
  sealed trait Type {
    def id: String
  }
  case object API_KEY extends Type { override val id: String = "apiKey" }
  case object HTTP extends Type { override val id: String = "http" }
  case object OAUTH2 extends Type { override val id: String = "oauth2" }
  case object OPEN_ID_CONNECT extends Type { override val id: String = "openIdConnect" }

  sealed trait In {
    def id: String
  }

  case object COOKIE extends In { override val id: String = "cookie" }
  case object HEADER extends In { override val id: String = "header" }
  case object QUERY extends In { override val id: String = "query" }
}

final case class SecurityScheme(
  node: MappingNode,
  `type`: Option[SecurityScheme.Type],
  description: Option[Scalar[String]],
  name: Option[Scalar[String]],
  in: Option[SecurityScheme.In],
  scheme: Option[Scalar[String]],
  bearerFormat: Option[Scalar[String]],
  flows: Option[OAuthFlows],
  openIdConnectUrl: Option[Scalar[String]],
  extensions: ScalarMap[String, Node]
)

final case class OAuthFlows(
  node: MappingNode,
  `implicit`: Option[OAuthFlow],
  password: Option[OAuthFlow],
  clientCredentials: Option[OAuthFlow],
  authorizationCode: Option[OAuthFlow]
)

final case class OAuthFlow(
  node: MappingNode,
  authorizationUrl: Option[Scalar[String]],
  tokenUrl: Option[Scalar[String]],
  refreshUrl: Option[Scalar[String]],
  scopes: Option[SecurityScopes],
  extensions: ScalarMap[String, Node]
)

final case class SecurityScopes(
  node: MappingNode,
  values: ScalarMap[String, Scalar[String]],
  extensions: ScalarMap[String, Node]
)
