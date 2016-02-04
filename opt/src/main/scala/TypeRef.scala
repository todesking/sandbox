package com.todesking.hoge

sealed abstract class TypeRef
object TypeRef {
  def from(c: Class[_]): Public = {
    if (c == java.lang.Integer.TYPE) Int
    else if (c == Long.javaClass) Long
    else if (c == Char.javaClass) Char
    else if (c == Byte.javaClass) Byte
    else if (c == Boolean.javaClass) Boolean
    else if (c == Short.javaClass) Short
    else if (c == Float.javaClass) Float
    else if (c == Double.javaClass) Double
    else if (c == Void.javaClass) Void
    else if (c.isArray) ???
    else Reference(ClassName(c.getName))
  }

  case object Undefined extends TypeRef
  case object This extends TypeRef
  case object Null extends TypeRef

  sealed abstract class Public extends TypeRef {
    def str: String
    def javaClass: Class[_]
  }

  sealed abstract class Primitive(override val str: String, override val javaClass: Class[_]) extends Public

  object Byte extends Primitive("B", java.lang.Byte.TYPE)
  object Boolean extends Primitive("Z", java.lang.Boolean.TYPE)
  object Char extends Primitive("C", java.lang.Character.TYPE)
  object Short extends Primitive("S", java.lang.Short.TYPE)
  object Int extends Primitive("I", java.lang.Byte.TYPE)
  object Float extends Primitive("F", java.lang.Integer.TYPE)
  object Long extends Primitive("J", java.lang.Long.TYPE)
  object Double extends Primitive("D", java.lang.Double.TYPE)
  object Void extends Primitive("V", java.lang.Void.TYPE)

  // TODO: Support CL
  case class Reference(className: ClassName) extends Public {
    override def str = s"L${className.binaryString};"
    // TODO: Support CL
    override val javaClass = Class.forName(className.str)
  }
}
