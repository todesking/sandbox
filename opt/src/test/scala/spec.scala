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
      else if(a > -10) -10
      else -100
  }
}

class Spec extends FunSpec with Matchers {
  def dot(filename: String, b: MethodBody): Unit = {
    import java.nio.file._
    // Files.write(Paths.get(filename), b.toDot.getBytes("UTF-8"))
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

      val intMethod = LocalMethodRef("intMethod()I")
      val longMethod = LocalMethodRef("longMethod()J")
      val ri = Instance.Rewritten(i, Map(
        intMethod -> i.methodBody(intMethod).get,
        longMethod -> i.methodBody(longMethod).get
      ))
      ri.instance.intMethod() should be(1)
      ri.instance.longMethod() should be(0L)
    }
    it("invokeVirtual0") {
      val d = new Test.InvokeVirtual0
      d.foo() should be(1)

      val i = Instance.Native(d)
      val foo = LocalMethodRef("foo()I")

      val ri = Instance.Rewritten(i, Map(foo -> i.methodBody(foo).get))

      ri.instance.foo() should be(1)
    }
    it("invokeVirtual1") {
      val d = new Test.InvokeVirtual1
      d.foo() should be(1)

      val i = Instance.Native(d)
      val foo = LocalMethodRef("foo()I")

      val ri = Instance.Rewritten(i, Map(foo -> i.methodBody(foo).get))

      ri.instance.foo() should be(1)
    }
    it("if") {
      val d = new Test.If
      d.foo(1) should be(100)
      d.foo(-1) should be(-10)
      d.foo(-11) should be(-100)

      val i = Instance.Native(d)
      val foo = LocalMethodRef("foo(I)I")

      val ri = Instance.Rewritten(i, Map(foo -> i.methodBody(foo).get))
      ri.instance.foo(1) should be(100)
      ri.instance.foo(-1) should be(-10)
      ri.instance.foo(-11) should be(-100)
    }
  }
}
