package org.github.mitallast.openapi.protobuf

import java.nio.file.{Files, Paths, StandardOpenOption}

import io.swagger.v3.parser.OpenAPIV3Parser
import org.github.mitallast.openapi.protobuf.compiler.ProtoCompiler
import org.github.mitallast.openapi.protobuf.writer.Writer

object Main extends App {
  val path = "./petstore.yaml"
  val api = new OpenAPIV3Parser().read(path)
  val protoFile = ProtoCompiler.compile(api, path)
  val source = Writer.writeFile(protoFile)
  println(source)
  Files.write(
    Paths.get(protoFile.path.value),
    source.getBytes("UTF-8"),
    StandardOpenOption.CREATE,
    StandardOpenOption.TRUNCATE_EXISTING
  )
}
