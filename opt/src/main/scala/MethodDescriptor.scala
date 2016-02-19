package com.todesking.hoge

import java.lang.reflect.{ Method => JMethod }

case class MethodDescriptor(ret: TypeRef.Public, args: Seq[TypeRef.Public]) {
  def str: String = s"${args.map(_.str).mkString("(", "", ")")}${ret.str}"
  def isVoid: Boolean = ret == TypeRef.Void
}
object MethodDescriptor {
  def parse(src: String): MethodDescriptor =
    Parsers.parseMethodDescriptor(src)

  def from(m: JMethod): MethodDescriptor =
    MethodDescriptor(TypeRef.from(m.getReturnType), m.getParameterTypes.map(TypeRef.from).toSeq)
}
