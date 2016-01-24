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
      i.hasMethod("foo", "()I") should be(true)

      val foo = LocalMethodRef("foo", MethodDescriptor.parse("()I"))
      val fooBody = i.methodBody(foo).get
      fooBody.initialFrame.stack should be('empty)
      fooBody.initialFrame.locals.size should be(1)
      fooBody.data(fooBody.initialFrame.local(0)) should be(Data(TypeRef.This, Some(obj)))
      fooBody.returns.size should be(1)
      val ret = fooBody.returns(0)
      fooBody.data(ret.retVal) should be(Data(TypeRef.Int, Some(1)))
      fooBody.dataSource(ret.retVal) should be(Some(Instruction.Const(1, TypeRef.Int)))
    }
  }
}
