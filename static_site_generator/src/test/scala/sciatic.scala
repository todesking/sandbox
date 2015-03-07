package com.todesking.sciatic

import org.scalatest.Matchers._
import org.expecty.Expecty

import scala.util.{Success, Failure}


class SciaticTest extends org.scalatest.FunSpec {
  val expect = new Expecty
  describe("Slim") {
    describe("Parser(local AST)") {
      it("should parse") {
        val parser = new SlimParser
        expect { parser.parseLocal("").get == List(1) }
        parser.parseLocal("| hoge").get shouldEqual List(0 -> SlimAST.Text("hoge"))
        parser.parseLocal("  | fuga").get shouldEqual List(2 -> SlimAST.Text("fuga"))
        parser.parseLocal("""a href="hoge"""").get shouldEqual List(0 -> SlimAST.Tag("a", Map("href" -> """"hoge"""" )))
        parser.parseLocal("""div.foo #bar data-x="xxx"""").get shouldEqual List(0 -> SlimAST.Tag(
          "div", Map(
            "id" -> """"bar"""",
            "class" -> """"foo"""",
            "data-x" -> """"xxx""""
          )))
        parser.parseLocal("""div a=1""").get shouldEqual List(0 -> SlimAST.Tag("div", Map("a" -> "1")))
        parser.parseLocal("""= 1 + 1""").get shouldEqual List(0 -> SlimAST.Out("1 + 1"))
        parser.parseLocal("""- 1 + 1""").get shouldEqual List(0 -> SlimAST.Code("1 + 1"))
        parser.parseLocal("""a href=linkTo("foo")""").get shouldEqual List(0 -> SlimAST.Tag(
          "a", Map("href" -> "linkTo(\"foo\")")))
      }
    }
  }
}
