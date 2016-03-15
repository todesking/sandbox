package com.todesking.hoge

import com.todesking.scalapp.syntax._

import org.scalatest.{ FunSpec, Matchers, Failed }

class Spec extends FunSpec with Matchers {
  def dotBody(filename: String, self: Instance[_ <: AnyRef], b: MethodBody): Unit = {
    import java.nio.file._
    Files.write(Paths.get(filename), b.dataflow(self).toDot.getBytes("UTF-8"))
  }

  override def withFixture(test: NoArgTest) =
    super.withFixture(test) match {
      case o @ Failed(t: BytecodeTransformException) =>
        println("=== FAILED")
        println(t)
        println(t.methodBody.pretty)
        o
      case o @ Failed(t: InvalidClassException) =>
        println("=== INVALID CLASS")
        println(t.instance.pretty)
        o
      case o =>
        o
    }

  describe("opt") {
    val defaultCL = ClassLoader.getSystemClassLoader
    it("duplicate") {
      class Const {
        def intMethod(): Int = 1
        def longMethod(): Long = 0L
      }
      val obj = new Const
      val i = Instance.of(obj)
      i.hasVirtualMethod("intMethod()I") should be(true)

      val intMethod = MethodRef.parse("intMethod()I", defaultCL)
      val longMethod = MethodRef.parse("longMethod()J", defaultCL)
      val ri = i.duplicate[Const].materialized
      ri.value.intMethod() should be(1)
      ri.value.longMethod() should be(0L)
    }
    it("invokeVirtual with no arguments") {
      class InvokeVirtual0 {
        def foo(): Int = bar()
        def bar(): Int = 1
      }
      val d = new InvokeVirtual0
      d.foo() should be(1)

      val i = Instance.of(d)
      val foo = MethodRef.parse("foo()I", defaultCL)

      val ri = i.duplicate.materialized

      ri.value.foo() should be(1)
    }
    it("invokeVirtual1") {
      class InvokeVirtual1 {
        def foo(): Int = bar(1)
        def bar(n: Int): Int = n
      }
      val d = new InvokeVirtual1
      d.foo() should be(1)

      val i = Instance.of(d)
      val foo = MethodRef.parse("foo()I", defaultCL)

      val ri = i.duplicate.materialized

      ri.value.foo() should be(1)
    }
    it("if") {
      class If {
        def foo(a: Int): Int =
          if (a > 0) 100
          else if (a > -10) -10
          else -100
      }
      val d = new If
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
      val obj = new OtherMethod.B
      obj.foo() should be(99)
      val i = Instance.of(obj)
      val foo = MethodRef.parse("foo()I", defaultCL)
      val ri = i.duplicate.materialized
      ri.value.foo() should be(99)
    }
    it("real upcast") {
      abstract class A {
        def foo(): Int
        def bar(): Int = 10
      }
      final class B extends A {
        override def foo() = baz()
        override def bar() = 99
        def baz() = bar()
      }
      val obj = new B
      obj.foo() should be(99)
      val i = Instance.of[A](obj)
      val foo = MethodRef.parse("foo()I", defaultCL)
      val ri = i.duplicate[A].materialized
      dotBody("real_upcast.dot", ri, ri.methodBody(foo))
      classOf[A].isAssignableFrom(ri.value.getClass) should be(true)
      classOf[B].isAssignableFrom(ri.value.getClass) should be(false)
      ri.value.foo() should be(99)
    }
    it("simple dataflow compile") {
      class A {
        def foo(): Int = if (bar > 20) 1 else 2
        def bar(): Int = 10
      }

      val i = Instance.of(new A)
      i.value.foo() should be(2)

      val foo = MethodRef.parse("foo()I", defaultCL)

      val ri = i.duplicate.materialized

      dotBody("s.dot", ri, ri.methodBody(foo))

      ri.value.foo() should be(2)
    }
    it("primitive field") {
      class A {
        val foo = 10
      }
      val foo = MethodRef.parse("foo()I", defaultCL)

      val i = Instance.of(new A)
      i.value.foo should be(10)

      val ri = i.duplicate.materialized
      ri.value.foo should be(10)
    }
    it("field duplicate") {
      abstract class Base {
        def foo: Int
      }
      class A extends Base {
        val x = 1000
        override def foo = x
      }
      class B extends A {
      }
      val i = Instance.of(new B)
      i.value.foo should be(1000)

      val ri = i.duplicate[Base].materialized
      ri.value.foo should be(1000)
    }
    it("dupdup") {
      abstract class Base {
        def foo: Int
      }
      class A extends Base {
        val x = 1000
        override def foo = x
      }
      class B extends A {
      }
      val i = Instance.of(new B)
      i.value.foo should be(1000)

      val ri = i.duplicate[Base].duplicate[Base].materialized
      ri.value.foo should be(1000)
    }
    it("field fusion(thisMethod)") {
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
      val expected = 111
      val i = Instance.of(new A(new B))
      i.value.foo should be(expected)

      val dup = i.duplicate[Base]

      val ri = dup.materialized
      ri.value.foo should be(expected)

      val foo = MethodRef.parse("foo()I", defaultCL)
      // A.outer + Base.outer + b
      dup.fields.size should be(3)
      val (fc, ff) = dup.fields.keys.find(_._2.name.indexOf("__b") != -1).get
      dup.dataflow(dup.thisRef, foo).usedFieldsOf(dup).size should be(1)

      val fi = Transformer.fieldFusion1(dup, fc, ff)
      fi.dataflow(fi.thisRef, foo).usedFieldsOf(fi).size should be(0)
      fi.materialized.value.foo should be(expected)
    }
    it("field fusion(base)") {
      class Base(b: B) {
        def foo(): Int = b.bar()
      }
      class B {
        def bar(): Int = 999
      }
      val expected = 999
      val foo = MethodRef.parse("foo()I", defaultCL)

      val i = Instance.of(new Base(new B))
      i.value.foo() should be(expected)
      i.dataflow(i.thisRef, foo).usedFieldsOf(i).size should be(1)

      val dup = i.duplicate1
      dup.dataflow(foo).usedFieldsOf(dup).size should be(1)
      val (fc, ff) = dup.fields.keys.find(_._2.name == "b").get

      val fi = Transformer.fieldFusion1(dup, fc, ff)
      fi.dataflow(foo).usedFieldsOf(fi).size should be(0)
      println(fi.pretty)
    }
    it("field fusion(recursive)") {
      pending
      class A(b: B) {
        def foo(): Int = b.bar() + 1000
      }
      class B(c: C) {
        def bar(): Int = c.baz() + 1
      }
      class C {
        def baz(): Int = 999
      }
      val expected = 2000
      val a = new A(new B(new C))
      a.foo() should be(expected)
      val i = Instance.of(a)
      val fused = Transformer.fieldFusion.apply(i).get
      fused.materialized.value.foo() should be(expected)
    }
    // TODO: accept new instance as constant in SetterConstructor
  }
}
