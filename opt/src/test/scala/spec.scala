package com.todesking.hoge

import com.todesking.scalapp.syntax._

import org.scalatest._

object Test {
  class Const {
    def foo(): Int = 1
  }
}

class Spec extends FunSpec with Matchers {
  describe("opt") {
    it("hoge-") {
      val orig = new Test.Const
      val opti = Opt.optimize[Test.Const](orig)

      orig.foo() should be(1)
      opti.foo() should be(1)
    }
    it("class") {
      val obj = new Test.Const
      val i = Instance.Native(obj)
      i.klass.method("foo", "()I").map(_.nameAndTypeString) should be(Some("foo()I"))
    }
  }
}
