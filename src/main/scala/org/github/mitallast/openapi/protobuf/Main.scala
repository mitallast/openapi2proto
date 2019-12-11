package org.github.mitallast.openapi.protobuf

import java.nio.file.{Files, Paths, StandardOpenOption}

import io.swagger.v3.parser.OpenAPIV3Parser
import org.github.mitallast.openapi.protobuf.compiler.ProtoCompiler
import org.github.mitallast.openapi.protobuf.writer.Writer

object Main extends App {
  System.setProperty("org.slf4j.simpleLogger.showShortLogName", "true")

  args.toList match {
    case filepath :: Nil => compile(filepath)
    case _               => usage()
  }

  private def usage(): Unit =
    println("""
              |usage: openapi2proto petstore.yaml  
              |""".stripMargin)

  private def compile(filepath: String): Unit = {
    val api = new OpenAPIV3Parser().read(filepath)
    val protoFile = ProtoCompiler.compile(api, filepath)
    val source = Writer.writeFile(protoFile)
    println(source)
    Files.write(
      Paths.get(protoFile.path.value),
      source.getBytes("UTF-8"),
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
  }
}
