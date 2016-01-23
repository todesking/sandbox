package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }

object Main {
  def main(args: Array[String]): Unit = {
  }
}

object Opt {
  def optimize[A: ClassTag](orig: A): A = {
    orig
  }
}

sealed abstract class Instance[A] {
  def klass: Klass
}
object Instance {
  case class Native[A](value: A) extends Instance[A] {
    override lazy val klass = Klass.from(value.getClass)
  }
}

sealed abstract class TypeRef {
  def str: String
}
object TypeRef {
  def from(c: Class[_]): TypeRef = {
    if(c == java.lang.Integer.TYPE) Int
    else if(c == java.lang.Void.TYPE) Void
    else Reference(ClassName(c.getName))
  }

  sealed abstract class Primitive(override val str: String) extends TypeRef

  object Int extends Primitive("I")
  object Void extends Primitive("V")

  case class Reference(className: ClassName) extends TypeRef {
    override def str = s"L${className.binaryString};"
  }
}

case class ClassName(str: String) {
  def binaryString: String = str.replace("\\.", "/")
}
case class MethodDescriptor(ret: TypeRef, args: Seq[TypeRef]) {
  def str: String = s"${args.map(_.str).mkString("(", ",", ")")}${ret.str}"
}
object MethodDescriptor {
  def parse(src: String): MethodDescriptor = {
    if(src == "()I") MethodDescriptor(TypeRef.Int, Seq.empty)
    else ???
  }
}
case class LocalMethodRef(name: String, descriptor: MethodDescriptor)

case class Klass(name: ClassName, methods: Seq[Method]) {
  def method(name: String, descriptor: String): Option[Method] =
    method(name, MethodDescriptor.parse(descriptor))
  def method(name: String, descriptor: MethodDescriptor): Option[Method] =
    method(LocalMethodRef(name, descriptor))
  def method(ref: LocalMethodRef): Option[Method] =
    methods.find { m => m.localRef == ref }
}
object Klass {
  def from(javaClass: Class[_]): Klass = {
    val name = ClassName(javaClass.getName)

    val methods = javaClass.getMethods.map(Method.from)

    Klass(name, methods)
  }
}

case class Method(name: String, descriptor: MethodDescriptor) {
  def nameAndTypeString: String = s"${name}${descriptor.str}"
  def localRef: LocalMethodRef = LocalMethodRef(name, descriptor)
}
object Method {
  def from(m: java.lang.reflect.Method): Method = {
    Method(m.getName, MethodDescriptor(TypeRef.from(m.getReturnType), m.getParameterTypes.map(TypeRef.from)))
  }
}
