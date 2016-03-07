package com.todesking.hoge

import java.lang.reflect.{ Method => JMethod, Constructor }

case class MethodRef(name: String, descriptor: MethodDescriptor) {
  def str: String = name + descriptor.str
  def pretty: String = str
  override def toString = pretty
  def isInit: Boolean = name == "<init>"
  def isVoid: Boolean = descriptor.isVoid
  def args: Seq[TypeRef.Public] = descriptor.args
  def ret: TypeRef.Public = descriptor.ret
  def renamed(newName: String): MethodRef = copy(name = newName)
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
