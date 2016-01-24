package com.todesking.hoge

import com.todesking.scalapp.syntax._

import org.scalatest._

object Test {
  class Const {
    def intMethod(): Int = 1
    def longMethod(): Long = 0L
  }
}

class Spec extends FunSpec with Matchers {
  describe("opt") {
    it("hoge-") {
      val orig = new Test.Const
      val opti = Opt.optimize[Test.Const](orig)

      orig.intMethod() should be(1)
      opti.intMethod() should be(1)
      orig.longMethod() should be(0L)
      opti.longMethod() should be(0L)
    }
    it("class") {
      val obj = new Test.Const
      val i = Instance.Native(obj)
      i.hasMethod("intMethod", "()I") should be(true)

      val intMethod = LocalMethodRef("intMethod", MethodDescriptor.parse("()I"))
      val intBody = i.methodBody(intMethod).get
      intBody.initialFrame.stack should be('empty)
      intBody.initialFrame.locals.size should be(1)
      intBody.data(intBody.initialFrame.local(0)) should be(Data(TypeRef.This, None))
      intBody.returns.size should be(1)
      val ret = intBody.returns(0)
      intBody.data(ret.retVal) should be(Data(TypeRef.Int, Some(1)))
      intBody.dataSource(ret.retVal) should be(Some(Instruction.Const(TypeRef.Int, 1)))

      val longMethod = LocalMethodRef("longMethod", MethodDescriptor.parse("()J"))
      val longBody = i.methodBody(longMethod).get
      longBody.data(longBody.returns(0).retVal) should be(Data(TypeRef.Long, Some(0L)))


      val iconst1 = intBody.dataSource(intBody.returns(0).retVal).get
      val rewritten = i.replaceInstruction(intMethod, iconst1.label, Instruction.Const(TypeRef.Int, Some(2)))
      rewritten.instance().intMethod() should be(2)
    }
  }
}
