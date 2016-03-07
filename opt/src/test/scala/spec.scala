package com.todesking.hoge

import com.todesking.scalapp.syntax._

import org.scalatest.{ FunSpec, Matchers }

object Test {
  class Complex {
    @scala.annotation.tailrec
    final def foo(a: Int): Int =
      if (a > 0) 100
      else if (a > -10) bar(a)
      else foo(a + 100)
    def bar(a: Int) = a
  }
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
      if (a > 0) 100
      else if (a > -10) -10
      else -100
  }
  object OtherMethod {
    abstract class A {
      def foo(): Int
      def bar(): Int = 10
    }
    class B extends A {
      override def foo() = baz()
      override def bar() = 99
      def baz() = bar()
    }
  }
  object Upcast {
    abstract class A {
      def foo(): Int
      def bar(): Int = 10
    }
    final class B extends A {
      override def foo() = baz()
      override def bar() = 99
      def baz() = bar()
    }
  }
  object SimpleDataflow {
    class A {
      def foo(): Int = if (bar > 20) 1 else 2
      def bar(): Int = 10
    }
  }
  object PrimitiveField {
    class A {
      val foo = 10
    }
  }
  object FieldDuplicate {
    abstract class Base {
      def foo: Int
    }
    class A extends Base {
      val x = 1000
      override def foo = x
    }
    class B extends A {
    }
  }
  object FieldFusion {
    abstract class Base {
      def foo(): Int
    }
    class A(b: B) extends Base {
      override def foo(): Int = b.bar() + 10 + b.baz()
    }
    class B {
      private[this] val x = 1
      def bar(): Int = x
      def baz(): Int = bar() + bazzz()
      def bazzz() = 99
    }
  }
}

class Spec extends FunSpec with Matchers {
  def dotBody(filename: String, self: Instance[_ <: AnyRef], b: MethodBody): Unit = {
    import java.nio.file._
    Files.write(Paths.get(filename), b.dataflow(self).toDot.getBytes("UTF-8"))
  }
  describe("opt") {
    val defaultCL = ClassLoader.getSystemClassLoader
    it("dot test") {
      val foo = MethodRef.parse("foo(I)I", defaultCL)
      val i = Instance.of(new Test.Complex)
      dotBody("complex.dot", i, i.methodBody(foo).get)
    }
    it("const") {
      val orig = new Test.Const
      val opti = Opt.optimize[Test.Const](orig)

      orig.intMethod() should be(1)
      opti.intMethod() should be(1)
      orig.longMethod() should be(0L)
      opti.longMethod() should be(0L)
    }
    it("class") {
      val obj = new Test.Const
      val i = Instance.of(obj)
      i.hasVirtualMethod("intMethod()I") should be(true)

      val intMethod = MethodRef.parse("intMethod()I", defaultCL)
      val longMethod = MethodRef.parse("longMethod()J", defaultCL)
      val ri = i.duplicate[Test.Const].materialized
      ri.value.intMethod() should be(1)
      ri.value.longMethod() should be(0L)
    }
    it("invokeVirtual0") {
      val d = new Test.InvokeVirtual0
      d.foo() should be(1)

      val i = Instance.of(d)
      val foo = MethodRef.parse("foo()I", defaultCL)

      val ri = i.duplicate.materialized

      ri.value.foo() should be(1)
    }
    it("invokeVirtual1") {
      val d = new Test.InvokeVirtual1
      d.foo() should be(1)

      val i = Instance.of(d)
      val foo = MethodRef.parse("foo()I", defaultCL)

      val ri = i.duplicate.materialized

      ri.value.foo() should be(1)
    }
    it("if") {
      val d = new Test.If
      d.foo(1) should be(100)
      d.foo(-1) should be(-10)
      d.foo(-11) should be(-100)

      val i = Instance.of(d)
      val foo = MethodRef.parse("foo(I)I", defaultCL)

      val ri = i.duplicate.materialized
      ri.value.foo(1) should be(100)
      ri.value.foo(-1) should be(-10)
      ri.value.foo(-11) should be(-100)
    }
    it("other method") {
      val obj = new Test.OtherMethod.B
      obj.foo() should be(99)
      val i = Instance.of(obj)
      val foo = MethodRef.parse("foo()I", defaultCL)
      val ri = i.duplicate.materialized
      ri.value.foo() should be(99)
    }
    it("real upcast") {
      val obj = new Test.Upcast.B
      obj.foo() should be(99)
      val i = Instance.of[Test.Upcast.A](obj)
      val foo = MethodRef.parse("foo()I", defaultCL)
      val ri = i.duplicate[Test.Upcast.A].materialized
      dotBody("real_upcast.dot", ri, ri.methodBody(foo).get)
      classOf[Test.Upcast.A].isAssignableFrom(ri.value.getClass) should be(true)
      classOf[Test.Upcast.B].isAssignableFrom(ri.value.getClass) should be(false)
      ri.value.foo() should be(99)
    }
    it("simple dataflow compile") {
      import Test.SimpleDataflow.A

      val i = Instance.of(new A)
      i.value.foo() should be(2)

      val foo = MethodRef.parse("foo()I", defaultCL)

      val ri = i.duplicate.materialized

      dotBody("s.dot", ri, ri.methodBody(foo).get)

      ri.value.foo() should be(2)
    }
    it("primitive field") {
      import Test.PrimitiveField.A
      val foo = MethodRef.parse("foo()I", defaultCL)

      val i = Instance.of(new A)
      i.value.foo should be(10)

      val ri = i.duplicate.materialized
      ri.value.foo should be(10)
    }
    it("field duplicate") {
      import Test.FieldDuplicate._
      val i = Instance.of(new B)
      i.value.foo should be(1000)

      val ri = i.duplicate[Base].materialized
      ri.value.foo should be(1000)
    }
    it("dupdup") {
      import Test.FieldDuplicate._
      val i = Instance.of(new B)
      i.value.foo should be(1000)

      val ri = i.duplicate[Base].duplicate[Base].materialized
      ri.value.foo should be(1000)
    }
    it("field fusion") {
      val expected = 111
      import Test.FieldFusion._
      val i = Instance.of(new A(new B))
      i.value.foo should be(expected)

      val dup = i.duplicate[Base]

      val ri = dup.materialized
      ri.value.foo should be(expected)

      dup.fields.size should be(1)
      val (fc, ff) = dup.fields.keys.head
      val fi = Transformer.fieldFusion(dup, fc, ff)
      println(fi.pretty)
      fi.materialized.value.foo should be(expected)
      println(fi.pretty)
    }
    it("inner class with primitive field") {
      pending
      abstract class Base {
        def x: Int
        def foo: Int = x
      }
      class A extends Base {
        override val x = 1
      }

      val i = Instance.of(new A)
      i.value.foo should be(1)

      val ri = i.duplicate[Base].materialized
      ri.value.foo should be(1)
    }
  }
}
