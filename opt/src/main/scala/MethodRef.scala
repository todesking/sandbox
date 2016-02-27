package com.todesking.hoge

import java.lang.reflect.{ Method => JMethod, Constructor }

// TODO: add ClassRef
case class MethodRef(name: String, descriptor: MethodDescriptor) {
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
object MethodRef {
  def from(m: Constructor[_]): MethodRef =
    MethodRef("<init>", MethodDescriptor(TypeRef.Void, m.getParameterTypes.map(TypeRef.from)))

  def from(m: JMethod): MethodRef =
    MethodRef(m.getName, MethodDescriptor.from(m))
  def parse(src: String, cl: ClassLoader): MethodRef =
    parser(cl).parse(src)
  case class parser(classLoader: ClassLoader) {
    lazy val all = """([^(]+)(\(.+)""".r
    def parse(src: String): MethodRef =
      src match {
        case `all`(name, desc) =>
          MethodRef(name, MethodDescriptor.parse(desc, classLoader))
        case unk =>
          throw new IllegalArgumentException(s"Invalid method ref: ${unk}")
      }
  }
}
