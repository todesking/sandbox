package com.todesking.sciatic

import org.scalatest.Matchers._
import scala.util.{Success, Failure}

class SciaticTest extends org.scalatest.FunSpec {
  describe("Slim") {
    describe("Parser(local AST)") {
      it("a") {
        val parser = new SlimParser
        parser.parseLocal("") shouldEqual Success(List.empty)
        parser.parseLocal("| hoge") shouldEqual Success(List(0 -> SlimAST.Text("hoge")))
      }
    }
  }
}
