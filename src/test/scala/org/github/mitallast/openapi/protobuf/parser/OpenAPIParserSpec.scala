package org.github.mitallast.openapi.protobuf.parser

import java.util.Collections

import cats.effect.ExitCode
import org.github.mitallast.openapi.protobuf.common._
import org.scalatest._
import org.yaml.snakeyaml.DumperOptions
import org.yaml.snakeyaml.error.Mark
import org.yaml.snakeyaml.nodes.{MappingNode, ScalarNode, Tag}

class OpenAPIParserSpec extends FlatSpec with Matchers {
  val mark = new Mark("", 0, 0, 0, "".toCharArray, 0)
  val scalarNode = new ScalarNode(Tag.STR, true, "", mark, mark, DumperOptions.ScalarStyle.DOUBLE_QUOTED)
  val mappingNode = new MappingNode(Tag.MAP, true, Collections.emptyList(), mark, mark, DumperOptions.FlowStyle.AUTO)

  def run[T](app: Result[T]): Either[ExitCode, T] = {
    val (log, result) = app.value.run.unsafeRunSync()
    log.foreach(println)
    result
  }

  it should "parse $ref local" in {
    val scalar = Scalar(scalarNode, "#/components/schemas/Test")
    val $ref = ComponentsReference(Scalar(scalarNode, "Test"))
    run(OpenAPIParser.parseRef(scalar)) shouldBe Right($ref)
  }

  it should "parse $ref external" in {
    val scalar = Scalar(scalarNode, "external.yaml#/components/schemas/Test")
    val $ref = ComponentsReference(Scalar(scalarNode, "Test"))
    val ext = ExternalReference(Scalar(scalarNode, "external.yaml"), $ref)
    run(OpenAPIParser.parseRef(scalar)) shouldBe Right(ext)
  }

  it should "parse $ref external in directory" in {
    val scalar = Scalar(scalarNode, "directory/external.yaml#/components/schemas/Test")
    val $ref = ComponentsReference(Scalar(scalarNode, "Test"))
    val ext = ExternalReference(Scalar(scalarNode, "directory/external.yaml"), $ref)
    run(OpenAPIParser.parseRef(scalar)) shouldBe Right(ext)
  }
}
