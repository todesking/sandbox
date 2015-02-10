package com.todesking.pretty_printer

import org.scalatest.{FunSpec, Matchers}
import Matchers._

class PrettyPrinterSpec extends FunSpec {
  implicit class HereDoc(self: StringContext) {
    def here(args: Any*) =
      self.s(args: _*).stripMargin.replaceAll("""\A\n|\n +\Z""", "")
  }

  object Example {
    import Doc._
     def binop(l: String, op: String, r: String) =
       Group(Nest(2,
         Group(Text(l) ^| Text(op)) ^| Text(r)
       ))

     val cond = binop("a", "==", "b")
     val expr1 = binop("a", "<<", "2")
     val expr2 = binop("a", "+", "b")
     def ifthen(c: Doc, e1: Doc, e2: Doc) =
       Group(
         Group(Nest(2, Text("if") ^| c))
         ^| Group(Nest(2, Text("then") ^| e1))
         ^| Group(Nest(2, Text("else") ^| e2))
       )
     val doc = ifthen(cond, expr1, expr2)
  }

  describe("PrettyPrinter.pretty") {
    describe("Example") {
      it("should fit width") {
        PrettyPrinter.pretty(32, Example.doc) shouldEqual here"""
          |if a == b then a << 2 else a + b
        """
        PrettyPrinter.pretty(15, Example.doc) shouldEqual here"""
          |if a == b
          |then a << 2
          |else a + b
        """
        PrettyPrinter.pretty(10, Example.doc) shouldEqual here"""
          |if a == b
          |then
          |  a << 2
          |else a + b
        """
        PrettyPrinter.pretty(8, Example.doc) shouldEqual here"""
          |if
          |  a == b
          |then
          |  a << 2
          |else
          |  a + b
        """
        PrettyPrinter.pretty(7, Example.doc) shouldEqual here"""
          |if
          |  a ==
          |    b
          |then
          |  a <<
          |    2
          |else
          |  a + b
        """
        PrettyPrinter.pretty(6, Example.doc) shouldEqual here"""
          |if
          |  a ==
          |    b
          |then
          |  a <<
          |    2
          |else
          |  a +
          |    b
        """
      }
    }
  }
}
