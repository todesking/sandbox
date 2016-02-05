package com.todesking.hoge

sealed abstract class TypeRef {
  def isDoubleWord: Boolean = false
  def pretty: String = toString
}
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

  trait DoubleWord extends TypeRef {
    override def isDoubleWord = true
  }

  case object Undefined extends TypeRef {
    override def pretty = "undefined"
  }
  case object This extends TypeRef {
    override def pretty = "this"
  }
  case object Null extends TypeRef {
    override def pretty = "null"
  }

  sealed abstract class Public extends TypeRef {
    def str: String
    def javaClass: Class[_]
  }

  sealed abstract class Primitive(
    override val pretty: String,
    override val str: String,
    override val javaClass: Class[_]
  ) extends Public

  object Byte extends Primitive("int", "B", java.lang.Byte.TYPE)
  object Boolean extends Primitive("bool", "Z", java.lang.Boolean.TYPE)
  object Char extends Primitive("char", "C", java.lang.Character.TYPE)
  object Short extends Primitive("short", "S", java.lang.Short.TYPE)
  object Int extends Primitive("int", "I", java.lang.Byte.TYPE)
  object Float extends Primitive("float", "F", java.lang.Integer.TYPE)
  object Long extends Primitive("long", "J", java.lang.Long.TYPE) with DoubleWord
  object Double extends Primitive("double", "D", java.lang.Double.TYPE) with DoubleWord
  object Void extends Primitive("void", "V", java.lang.Void.TYPE)

  // TODO: Support CL
  case class Reference(className: ClassName) extends Public {
    override def str = s"L${className.binaryString};"
    // TODO: Support CL
    override val javaClass = Class.forName(className.str)
    override def pretty = className.str
  }
}
