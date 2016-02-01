package com.todesking.hoge

import com.todesking.scalapp.syntax._

import org.scalatest._

object Test {
  class Const {
    def intMethod(): Int = 1
    def longMethod(): Long = 0L
  }

  class InvokeVirtual0 {
    def foo(): Int = bar()
    def bar(): Int = 1
  }
  class InvokeVirtual1 {
    def foo(): Int = bar(1)
    def bar(n: Int): Int = n
  }
  class If {
    def foo(a: Int): Int =
      if(a > 0) 100
      else -100
  }
}

class Spec extends FunSpec with Matchers {
  def dot(filename: String, b: MethodBody): Unit = {
    import java.nio.file._
    Files.write(Paths.get(filename), b.toDot.getBytes("UTF-8"))
  }
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
      val rewritten = i.replaceInstruction(intMethod, iconst1.label, Instruction.Const(TypeRef.Int, 2))

      rewritten.methodBody(intMethod).get.data(rewritten.methodBody(intMethod).get.returns(0).retVal) should be(Data(TypeRef.Int, Some(2)))

      rewritten.instance().intMethod() should be(2)
    }
    it("invokeVirtual0") {
      val d = new Test.InvokeVirtual0
      d.foo() should be(1)

      val i = Instance.Native(d)
      val foo = LocalMethodRef("foo", MethodDescriptor.parse("()I"))

      val ri = i.replaceInstruction(foo, i.methodBody(foo).get.returns(0).label, Instruction.Return())

      ri.instance.foo() should be(1)
    }
    it("invokeVirtual1") {
      val d = new Test.InvokeVirtual1
      d.foo() should be(1)

      val i = Instance.Native(d)
      val foo = LocalMethodRef("foo", MethodDescriptor.parse("()I"))

      val ri = i.replaceInstruction(foo, i.methodBody(foo).get.returns(0).label, Instruction.Return())

      ri.instance.foo() should be(1)
    }
    it("if") {
      val d = new Test.If
      d.foo(1) should be(100)
      d.foo(-1) should be(-100)

      val i = Instance.Native(d)
      val foo = LocalMethodRef("foo", MethodDescriptor.parse("(I)I"))

      val ri = i.replaceInstruction(
        foo,
        i.methodBody(foo).get.instructions.find {
          case Instruction.Const(TypeRef.Int, 100) => true
          case _ => false
        }.get.label,
        Instruction.Const(TypeRef.Int, 500))

      dot("if.dot", ri.methodBody(foo).get)

      pending
      ri.instance.foo(1) should be(500)
      ri.instance.foo(-1) should be(-100)
    }
  }
}
