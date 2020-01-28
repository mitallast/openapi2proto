package org.github.mitallast.openapi.protobuf.writer

import org.github.mitallast.openapi.protobuf.model.ProtoFile

object Writer {
  import instances.ProtoFileWriter

  def writeFile(file: ProtoFile): String = write(file)

  def write[T](t: T)(implicit writer: Writer[T]): String = {
    val builder = new StringBuilder()
    writer.write(t, builder)
    builder.toString()
  }
}

sealed trait Writer[T] {
  def write(value: T, builder: StringBuilder): Unit
}

object instances {
  import org.github.mitallast.openapi.protobuf.model._
  import org.github.mitallast.openapi.protobuf.model.lexical._

  case object offset
  case object newline
  case object end

  private implicit class StringBuilderWriter(val builder: StringBuilder) {
    def <<[T](value: T)(implicit writer: Writer[T]): StringBuilderWriter = {
      writer.write(value, builder)
      this
    }
  }

  private def instance[T](f: (T, StringBuilder) => Unit): Writer[T] = new Writer[T] {
    override def write(value: T, builder: StringBuilder): Unit = f(value, builder)
  }

  implicit val OffsetWriter: Writer[offset.type] = instance((_, builder) => builder.append("   "))
  implicit val NewlineWriter: Writer[newline.type] = instance((_, builder) => builder.append("\n"))
  implicit val EndWriter: Writer[end.type] = instance((_, builder) => builder.append(";"))
  implicit val RawStringWriter: Writer[String] = instance((raw, builder) => builder.append(raw))
  implicit val RawIntWriter: Writer[Int] = instance((raw, builder) => builder.append(raw))

  implicit def vectorWriter[T](implicit writer: Writer[T]): Writer[Vector[T]] = instance { (values, builder) =>
    if (values.nonEmpty) {
      for (elem <- values) {
        builder << elem
      }
      builder << newline
    }
  }

  implicit val TypeIdentWriter: Writer[TypeIdentifier] = instance {
    case (id: Identifier, builder)     => builder << id
    case (id: FullIdentifier, builder) => builder << id
  }

  implicit val IdentWriter: Writer[Identifier] = instance { (id, builder) =>
    builder.append(id.value)
  }

  implicit val FullIdentWriter: Writer[FullIdentifier] = instance { (id, builder) =>
    builder.append(id.value)
  }

  implicit val ConstantValueWriter: Writer[ConstantValue] = instance {
    case (c: StringValue, builder) =>
      val stmt = s""""${c.value}""""
      valid(stmt, letters.strLit)
      builder.append(stmt)
    case (c: LongValue, builder)    => builder.append(c.value)
    case (c: DoubleValue, builder)  => builder.append(c.value)
    case (c: BooleanValue, builder) => builder.append(c.value)
  }

  implicit val ProtoSyntaxWriter: Writer[ProtoSyntax] = instance { (_, builder) =>
    builder << """syntax = "proto3"""" << end << newline
  }

  implicit val OptionStatementWriter: Writer[OptionStatement] = instance { (option, builder) =>
    builder << "option "
    option.optionName match {
      case Identifier(id)     => builder << id
      case FullIdentifier(id) => builder << "(" << id << ")"
    }
    builder << " = " << option.value << end << newline
  }

  implicit val EnumOptionWriter: Writer[EnumOption] = instance { (option, builder) =>
    builder << offset << offset << "option " << option.optionName << " = " << option.value << end << newline
  }

  implicit val EnumValueOptionWriter: Writer[EnumValueOption] = instance { (option, builder) =>
    builder << option.optionName << " = " << option.value
  }

  implicit val MessageOptionWriter: Writer[MessageOption] = instance { (option, builder) =>
    builder << offset << "option " << option.optionName << " = " << option.value << end << newline
  }

  implicit val ServiceOptionWriter: Writer[ServiceOption] = instance { (option, builder) =>
    builder << offset << "option " << option.optionName << " = " << option.value << end << newline
  }

  implicit val FieldOptionWriter: Writer[FieldOption] = instance { (option, builder) =>
    builder << option.optionName << " = " << option.value
  }

  implicit val FieldOptionsWriter: Writer[Vector[FieldOption]] = instance { (options, builder) =>
    if (options.nonEmpty) {
      builder << " [ "
      for (option <- options) {
        builder << option << " "
      }
      builder << "]"
    }
  }

  implicit val ProtoFileWriter: Writer[ProtoFile] = instance { (file, builder) =>
    builder << file.syntax
    builder << newline
    builder << "package " << file.packageName << end << newline
    builder << newline

    builder << file.imports
    builder << file.options

    if (file.messages.nonEmpty) {
      for (message <- file.messages) {
        builder << message
        builder << newline
      }
    }
    if (file.services.nonEmpty) {
      for (service <- file.services) {
        builder << newline
        builder << service
      }
    }
  }

  implicit val ImportStatementWriter: Writer[ImportStatement] = instance { (imports, builder) =>
    val path = s""""${imports.path.value}""""
    valid(path, letters.strLit)
    builder << "import " << path << end << newline
  }

  implicit val EnumWriter: Writer[Enum] = instance { (enum, builder) =>
    builder << offset << "enum " << enum.enumName << " {" << newline
    builder << enum.options
    for (value <- enum.values) {
      builder << value
    }
    builder << offset << "}" << newline
  }

  implicit val EnumValueWriter: Writer[EnumValue] = instance { (value, builder) =>
    builder << offset << offset << value.identifier << " = " << value.value
    if (value.options.nonEmpty) {
      builder << " [ "
      for (option <- value.options) {
        builder << option << " "
      }
      builder << "]"
    }
    builder << end << newline
  }

  implicit val MessageWriter: Writer[Message] = instance { (message, builder) =>
    builder << "message " << message.messageName << " {" << newline
    if (message.reserved.nonEmpty) {
      builder << offset << "reserved " << message.reserved.mkString(", ") << end << newline
      builder << newline
    }
    builder << message.options

    if (message.enums.nonEmpty) {
      for (enum <- message.enums) {
        builder << enum
      }
      if (message.fields.nonEmpty) {
        builder << newline
      }
    }

    for (field <- message.fields) {
      builder << field
    }
    builder << "}" << newline
  }

  implicit val MessageFieldWriter: Writer[MessageField] = instance {
    case (field: NormalField, builder)   => builder << field
    case (field: RepeatedField, builder) => builder << field
    case (field: MapField, builder)      => builder << field
    case (field: OneOf, builder)         => builder << field
  }

  implicit val NormalFieldWriter: Writer[NormalField] = instance { (f, builder) =>
    builder << offset << f.fieldType << " " << f.fieldName << " = " << f.number
    builder << f.options << end << newline
  }

  implicit val RepeatedFieldWriter: Writer[RepeatedField] = instance { (f, builder) =>
    builder << offset << "repeated " << f.fieldType << " " << f.fieldName << " = " << f.number
    builder << f.options << end << newline
  }

  implicit val MapFieldWriter: Writer[MapField] = instance { (m, builder) =>
    builder << offset << "map<" << m.keyType << ", " << m.valueType << "> " << m.fieldName << " = " << m.number
    builder << m.options << end << newline
  }

  implicit val OneOfWriter: Writer[OneOf] = instance { (o, builder) =>
    builder << offset << "oneof " << o.oneOfName << " {" << newline
    for (field <- o.fields) {
      builder << field
    }
    builder << offset << "}" << newline
  }

  implicit val OneOfFieldWriter: Writer[OneOfField] = instance { (f, builder) =>
    builder << offset << offset << f.fieldType << " " << f.fieldName << " = " << f.number
    builder << f.options << end << newline
  }

  implicit val ServiceWriter: Writer[Service] = instance { (service, builder) =>
    builder << "service " << service.serviceName << " {" << newline
    builder << service.options
    for (rpc <- service.rpc) {
      builder << rpc
    }
    builder << "}" << newline
  }

  implicit val RpcStatementWriter: Writer[RpcStatement] = instance { (rpc, builder) =>
    builder << newline
    builder << offset << "rpc " << rpc.rpcName << " (" << rpc.requestType << ") returns (" << rpc.responseType << ")"
    if (rpc.options.nonEmpty) {
      builder << " {" << newline
      for (option <- rpc.options) {
        builder << option
      }
      builder << offset << "}" << newline
    } else {
      builder << end << newline
    }
  }

  implicit val RpcOptionWriter: Writer[RpcOption] = instance { (opt, builder) =>
    builder << offset << offset << "option (" << opt.identifier << ") = { "
    for (o <- opt.options) {
      builder << o.optionName << ": " << o.value << " "
    }
    builder << "}" << end << newline
  }
}
