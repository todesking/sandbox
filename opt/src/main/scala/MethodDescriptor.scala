package com.todesking.hoge

import java.lang.reflect.{ Method => JMethod, Constructor }

case class MethodDescriptor(ret: TypeRef.Public, args: Seq[TypeRef.Public]) {
  def str: String = s"${args.map(_.str).mkString("(", "", ")")}${ret.str}"
  def isVoid: Boolean = ret == TypeRef.Void
}
object MethodDescriptor {
  def parse(src: String, cl: ClassLoader): MethodDescriptor =
    Parsers.parseMethodDescriptor(src, cl)

  def from(m: JMethod): MethodDescriptor =
    MethodDescriptor(TypeRef.from(m.getReturnType), m.getParameterTypes.map(TypeRef.from).toSeq)

  def from(m: Constructor[_]): MethodDescriptor =
    MethodDescriptor(TypeRef.Void, m.getParameterTypes.map(TypeRef.from).toSeq)
}
