package org.github.mitallast.openapi.protobuf.model

import java.util.concurrent.atomic.AtomicInteger

import lexical._

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait ProtoSyntax
case object ProtoSyntaxV3 extends ProtoSyntax

final case class ProtoPath(value: String) {
  valid(value, identifiers.protoPath)
}

sealed trait TypeIdentifier {
  def value: String
}
final case class Identifier(value: String) extends TypeIdentifier {
  require(!value.isEmpty, "empty identifier is not allowed")
  valid(value, identifiers.ident)
}
object FullIdentifier {
  val empty: FullIdentifier = FullIdentifier("google.protobuf.Empty")
}
final case class FullIdentifier(value: String) extends TypeIdentifier {
  require(!value.isEmpty, "empty identifier is not allowed")
  valid(value, identifiers.fullIdent)
}

object ProtoFile {
  def builder(packageName: FullIdentifier): ProtoFileBuilder =
    builder(ProtoPath(s"${packageName.value}.proto"), packageName)

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
  enums: Vector[Enum],
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
  private val enums = Vector.newBuilder[Enum]
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
  def +=(enum: Enum): ProtoFileBuilder = {
    enums += enum
    this
  }
  def +=(message: Message): ProtoFileBuilder = {
    require(
      !registeredMessages.contains(message.messageName),
      s"${message.messageName.value}: message with same identifier already registered"
    )
    registeredMessages += message.messageName
    messages += message
    this
  }
  def +=(service: Service): ProtoFileBuilder = {
    services += service
    this
  }

  def build: ProtoFile =
    new ProtoFile(
      syntax,
      path,
      packageName,
      options.result(),
      imports.result(),
      enums.result(),
      messages.result(),
      services.result()
    )
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

  private def use(num: Int): Unit = {
    require(!used.contains(num))
    used += num
  }

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

  def +=(value: EnumValue): EnumBuilder = {
    use(value.value)
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

  private val used = mutable.Set.empty[Int]
  private val counter = new AtomicInteger()

  private def use(num: Int): Unit = {
    require(!used.contains(num))
    used += num
  }

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

  def +=(field: MessageField): MessageBuilder = {
    field match {
      case n: NormalField   => use(n.number)
      case r: RepeatedField => use(r.number)
      case m: MapField      => use(m.number)
      case o: OneOf         => for (f <- o.fields) use(f.number)
    }
    fields += field
    this
  }

  def build: Message = Message(messageName, options.result(), reserved.result(), fields.result())
}

object Message {
  def builder(messageName: Identifier): MessageBuilder = new MessageBuilder(messageName)
}

final case class Message(
  messageName: Identifier,
  options: Vector[MessageOption],
  reserved: Vector[Int],
  fields: Vector[MessageField]
)

final case class MessageOption(optionName: Identifier, value: ConstantValue)

sealed trait MessageField

final case class FieldOption(optionName: Identifier, value: ConstantValue)

final case class NormalField(
  fieldType: TypeIdentifier,
  fieldName: Identifier,
  number: Int,
  options: Vector[FieldOption]
) extends MessageField

final case class RepeatedField(
  fieldType: TypeIdentifier,
  fieldName: Identifier,
  number: Int,
  options: Vector[FieldOption]
) extends MessageField

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
  def apply(identifier: Identifier, value: String): OptionStatement =
    OptionStatement(identifier, StringValue(value))
  def apply(identifier: Identifier, value: Long): OptionStatement =
    OptionStatement(identifier, LongValue(value))
  def apply(identifier: Identifier, value: Double): OptionStatement =
    OptionStatement(identifier, DoubleValue(value))
  def apply(identifier: Identifier, value: Boolean): OptionStatement =
    OptionStatement(identifier, BooleanValue(value))
}

final case class OptionStatement(optionName: Identifier, value: ConstantValue)

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
