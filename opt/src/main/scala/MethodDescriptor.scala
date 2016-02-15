package com.todesking.hoge

import java.lang.reflect.{ Method => JMethod }

case class MethodDescriptor(ret: TypeRef.Public, args: Seq[TypeRef.Public]) {
  def str: String = s"${args.map(_.str).mkString("(", "", ")")}${ret.str}"
  def isVoid: Boolean = ret == TypeRef.Void
}
object MethodDescriptor {
  def parse(src: String): MethodDescriptor =
    Parser.parse(src)

  def from(m: JMethod): MethodDescriptor =
    MethodDescriptor(TypeRef.from(m.getReturnType), m.getParameterTypes.map(TypeRef.from).toSeq)

  object Parser extends scala.util.parsing.combinator.RegexParsers {
    def parse(src: String): MethodDescriptor =
      parseAll(all, src).get
    lazy val all = args ~ tpe ^^ { case args ~ ret => MethodDescriptor(ret, args) }
    lazy val args = ('(' ~> rep(tpe)) <~ ')'
    val refPat = """L([^;]+);""".r
    lazy val tpe = "B|Z|C|S|I|F|J|D|V|L[^;]+;".r ^^ {
      case "B" => TypeRef.Byte
      case "Z" => TypeRef.Boolean
      case "C" => TypeRef.Char
      case "S" => TypeRef.Short
      case "I" => TypeRef.Int
      case "F" => TypeRef.Float
      case "J" => TypeRef.Long
      case "D" => TypeRef.Double
      case "V" => TypeRef.Void
      case `refPat`(ref) =>
        val cName = ref.replaceAll("/", ".")
        val cl = ClassLoader.getSystemClassLoader
        val klass = cl.loadClass(cName)
        TypeRef.Reference(ClassRef(cName, klass.getClassLoader))
    }
  }
}
