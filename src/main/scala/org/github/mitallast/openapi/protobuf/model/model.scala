package org.github.mitallast.openapi.protobuf.model

import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait ProtoSyntax
case object ProtoSyntaxV3 extends ProtoSyntax

final case class ProtoPath(value: String)

sealed trait TypeIdentifier {
  def value: String
}
object Identifier {
  lazy val body: Identifier = Identifier("body")
  lazy val response_body: Identifier = Identifier("response_body")
  lazy val string: Identifier = Identifier("string")
  lazy val bytes: Identifier = Identifier("bytes")
  lazy val int32: Identifier = Identifier("int32")
  lazy val int64: Identifier = Identifier("int64")
  lazy val fixed32: Identifier = Identifier("fixed32")
  lazy val fixed64: Identifier = Identifier("fixed64")
  lazy val double: Identifier = Identifier("double")
  lazy val float: Identifier = Identifier("float")
  lazy val bool: Identifier = Identifier("bool")
}
final case class Identifier(value: String) extends TypeIdentifier
object FullIdentifier {
  lazy val int32: FullIdentifier = FullIdentifier("google.protobuf.Int32Value")
  lazy val int64: FullIdentifier = FullIdentifier("google.protobuf.Int64Value")
  lazy val float: FullIdentifier = FullIdentifier("google.protobuf.FloatValue")
  lazy val double: FullIdentifier = FullIdentifier("google.protobuf.DoubleValue")
  lazy val string: FullIdentifier = FullIdentifier("google.protobuf.StringValue")
  lazy val bool: FullIdentifier = FullIdentifier("google.protobuf.BoolValue")
  lazy val empty: FullIdentifier = FullIdentifier("google.protobuf.Empty")
  lazy val http: FullIdentifier = FullIdentifier("google.api.http")
  lazy val timestamp: FullIdentifier = FullIdentifier("google.protobuf.Timestamp")

  def apply(value: String): FullIdentifier = new FullIdentifier(value)
  def apply(wrapper: Identifier, nested: Identifier): FullIdentifier =
    new FullIdentifier(s"${wrapper.value}.${nested.value}")
}
final case class FullIdentifier(value: String) extends TypeIdentifier

object ProtoFile {
  def builder(path: ProtoPath, packageName: FullIdentifier): ProtoFileBuilder =
    builder(ProtoSyntaxV3, path, packageName)

  def builder(syntax: ProtoSyntax, path: ProtoPath, packageName: FullIdentifier): ProtoFileBuilder =
    new ProtoFileBuilder(syntax: ProtoSyntax, path, packageName)
}

final case class ProtoFile(
  syntax: ProtoSyntax,
  path: ProtoPath,
  packageName: FullIdentifier,
  options: Vector[OptionStatement],
  imports: Vector[ImportStatement],
  messages: Vector[Message],
  services: Vector[Service]
)

final class ProtoFileBuilder(
  private val syntax: ProtoSyntax,
  private val path: ProtoPath,
  private val packageName: FullIdentifier
) {
  private val options = Vector.newBuilder[OptionStatement]
  private val imports = Vector.newBuilder[ImportStatement]
  private val messages = Vector.newBuilder[Message]
  private val services = Vector.newBuilder[Service]

  private val registeredMessages = mutable.Set.empty[Identifier]

  def +=(option: OptionStatement): ProtoFileBuilder = {
    options += option
    this
  }
  def +=(importStatement: ImportStatement): ProtoFileBuilder = {
    imports += importStatement
    this
  }

  def contains(id: Identifier): Boolean = registeredMessages.contains(id)

  def +=(message: Message): ProtoFileBuilder = {
    registeredMessages += message.messageName
    messages += message
    this
  }

  def +=(service: Service): ProtoFileBuilder = {
    services += service
    this
  }

  def build: ProtoFile =
    new ProtoFile(syntax, path, packageName, options.result(), imports.result(), messages.result(), services.result())
}

object ImportStatement {
  def apply(path: String): ImportStatement = apply(ProtoPath(path))
  def apply(path: ProtoPath): ImportStatement = new ImportStatement(path)
}
final case class ImportStatement(path: ProtoPath)

final class EnumBuilder(private val enumName: Identifier) {
  private val options = Vector.newBuilder[EnumOption]
  private val values = Vector.newBuilder[EnumValue]

  private val used = mutable.Set.empty[Int]
  private val counter = new AtomicInteger()

  @tailrec
  def nextValueNum: Int = {
    val id = counter.getAndIncrement()
    if (used.contains(id)) {
      nextValueNum
    } else id
  }

  def +=(option: EnumOption): EnumBuilder = {
    options += option
    this
  }

  def contains(num: Int): Boolean = used.contains(num)

  def +=(value: EnumValue): EnumBuilder = {
    used += value.value
    values += value
    this
  }

  def build: Enum = Enum(enumName, options.result(), values.result())
}

object Enum {
  def builder(enumName: Identifier): EnumBuilder = new EnumBuilder(enumName)
}
final case class Enum(enumName: Identifier, options: Vector[EnumOption], values: Vector[EnumValue])

final case class EnumOption(optionName: TypeIdentifier, value: ConstantValue)

final case class EnumValue(identifier: Identifier, value: Int, options: Vector[EnumValueOption])

final case class EnumValueOption(optionName: TypeIdentifier, value: ConstantValue)

final class MessageBuilder(private val messageName: Identifier) {
  private val options = Vector.newBuilder[MessageOption]
  private val reserved = Vector.newBuilder[Int]
  private val fields = Vector.newBuilder[MessageField]
  private val enums = Vector.newBuilder[Enum]

  private val used = mutable.Set.empty[Int]
  private val counter = new AtomicInteger()

  @tailrec
  def nextFieldNum: Int = {
    val id = counter.incrementAndGet()
    if (used.contains(id)) {
      nextFieldNum
    } else id
  }

  def +=(option: MessageOption): MessageBuilder = {
    options += option
    this
  }

  def reserved(id: Int): MessageBuilder = {
    reserved += id
    used += id
    this
  }

  def reserved(id: Seq[Int]): MessageBuilder = {
    reserved ++= id
    used ++= id
    this
  }

  def use(id: Int): Unit = used += id

  def contains(id: Int): Boolean = used.contains(id)

  def +=(field: MessageField): MessageBuilder = {
    fields += field
    this
  }

  def +=(enum: Enum): Unit = enums += enum

  def build: Message = Message(
    messageName = messageName,
    options = options.result(),
    reserved = reserved.result(),
    fields = fields.result(),
    enums = enums.result()
  )
}

object Message {
  def builder(messageName: Identifier): MessageBuilder = new MessageBuilder(messageName)
}

final case class Message(
  messageName: Identifier,
  options: Vector[MessageOption],
  reserved: Vector[Int],
  fields: Vector[MessageField],
  enums: Vector[Enum]
)

final case class MessageOption(optionName: Identifier, value: ConstantValue)

sealed trait MessageField

final case class FieldOption(optionName: Identifier, value: ConstantValue)

final case class NormalField(
  fieldType: TypeIdentifier,
  fieldName: Identifier,
  number: Int,
  options: Vector[FieldOption] = Vector.empty
) extends MessageField

final case class RepeatedField(
  fieldType: TypeIdentifier,
  fieldName: Identifier,
  number: Int,
  options: Vector[FieldOption] = Vector.empty
) extends MessageField

final class OneOfBuilder(private val oneOfName: Identifier) {
  private val fields = Vector.newBuilder[OneOfField]

  def +=(field: OneOfField): OneOfBuilder = {
    fields += field
    this
  }

  def build: OneOf = OneOf(oneOfName, fields.result())
}
object OneOf {
  def builder(oneOfName: Identifier): OneOfBuilder = new OneOfBuilder(oneOfName)
}
final case class OneOf(oneOfName: Identifier, fields: Vector[OneOfField]) extends MessageField

final case class OneOfField(fieldType: TypeIdentifier, fieldName: Identifier, number: Int, options: Vector[FieldOption])

final case class MapField(
  keyType: Identifier,
  valueType: FullIdentifier,
  fieldName: Identifier,
  number: Int,
  options: Vector[FieldOption]
) extends MessageField

final class ServiceBuilder(private val serviceName: Identifier) {
  private val options = Vector.newBuilder[ServiceOption]
  private val rpc = Vector.newBuilder[RpcStatement]

  def +=(option: ServiceOption): ServiceBuilder = {
    options += option
    this
  }
  def +=(op: RpcStatement): ServiceBuilder = {
    rpc += op
    this
  }

  def build: Service = Service(serviceName, options.result(), rpc.result())
}

object Service {
  def builder(serviceName: Identifier): ServiceBuilder = new ServiceBuilder(serviceName)
}
final case class Service(serviceName: Identifier, options: Vector[ServiceOption], rpc: Vector[RpcStatement])

final case class ServiceOption(optionName: Identifier, value: ConstantValue)

final class RpcBuilder(private val rpcName: Identifier) {
  private var requestType: TypeIdentifier = FullIdentifier.empty
  private var responseType: TypeIdentifier = FullIdentifier.empty
  private val options = Vector.newBuilder[RpcOption]

  def withRequestType(identifier: TypeIdentifier): RpcBuilder = {
    requestType = identifier
    this
  }
  def withResponseType(identifier: TypeIdentifier): RpcBuilder = {
    responseType = identifier
    this
  }
  def +=(op: RpcOption): RpcBuilder = {
    options += op
    this
  }

  def build: RpcStatement = RpcStatement(rpcName, requestType, responseType, options.result())
}

object RpcStatement {
  def builder(rpcName: Identifier): RpcBuilder = new RpcBuilder(rpcName)
}

final case class RpcStatement(
  rpcName: Identifier,
  requestType: TypeIdentifier,
  responseType: TypeIdentifier,
  options: Vector[RpcOption]
)

final class RpcOptionBuilder(private val identifier: FullIdentifier) {
  private val options = Vector.newBuilder[OptionStatement]

  def +=(op: OptionStatement): RpcOptionBuilder = {
    options += op
    this
  }

  def build: RpcOption = RpcOption(identifier, options.result())
}

object RpcOption {
  def builder(identifier: FullIdentifier) = new RpcOptionBuilder(identifier)
}

final case class RpcOption(identifier: FullIdentifier, options: Vector[OptionStatement])

object OptionStatement {
  def apply(identifier: TypeIdentifier, value: String): OptionStatement =
    OptionStatement(identifier, StringValue(value))
  def apply(identifier: TypeIdentifier, value: Long): OptionStatement =
    OptionStatement(identifier, LongValue(value))
  def apply(identifier: TypeIdentifier, value: Double): OptionStatement =
    OptionStatement(identifier, DoubleValue(value))
  def apply(identifier: TypeIdentifier, value: Boolean): OptionStatement =
    OptionStatement(identifier, BooleanValue(value))
}

final case class OptionStatement(optionName: TypeIdentifier, value: ConstantValue)

object ConstantValue {
  def apply(value: String): ConstantValue = StringValue(value)
  def apply(value: Long): ConstantValue = LongValue(value)
  def apply(value: Double): ConstantValue = DoubleValue(value)
  def apply(value: Boolean): ConstantValue = BooleanValue(value)
}

sealed trait ConstantValue
final case class StringValue(value: String) extends ConstantValue
final case class LongValue(value: Long) extends ConstantValue
final case class DoubleValue(value: Double) extends ConstantValue
final case class BooleanValue(value: Boolean) extends ConstantValue
