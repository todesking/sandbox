package com.todesking.hoge

import java.lang.reflect.{ Method => JMethod }

case class LocalMethodRef(name: String, descriptor: MethodDescriptor) {
  def str: String = name + descriptor.str
  def isVoid: Boolean = descriptor.isVoid
  def args: Seq[TypeRef.Public] = descriptor.args
  def ret: TypeRef.Public = descriptor.ret
  // TODO: support protected/package private(use getDeclaredMethods() for each superclasses)
  def getJavaMethod(klass: Class[_]): Option[JMethod] =
    klass.getMethods.find { m =>
      m.getName == name &&
        m.getParameterTypes.size == args.size &&
        m.getParameterTypes.zip(args.map(_.javaClass)).forall { case (p1, p2) => p1 == p2 }
    }
}
object LocalMethodRef {
  def from(m: JMethod): LocalMethodRef =
    LocalMethodRef(m.getName, MethodDescriptor.from(m))
  def apply(src: String): LocalMethodRef =
    Parser.parse(src)
  object Parser {
    lazy val all = """([^(]+)(\(.+)""".r
    def parse(src: String): LocalMethodRef =
      src match {
        case `all`(name, desc) =>
          LocalMethodRef(name, MethodDescriptor.parse(desc))
        case unk =>
          throw new IllegalArgumentException(s"Invalid method ref: ${unk}")
      }
  }
}
