package com.todesking.hoge

object Parsers extends scala.util.parsing.combinator.RegexParsers {
  def parseTypeRef(src: String): TypeRef.Public =
    parseAll(typeRef.all, src).get

  def parseMethodDescriptor(src: String): MethodDescriptor =
    parseAll(methodDescriptor.all, src).get

  def parseFieldDescriptor(src: String): FieldDescriptor =
    parseAll(fieldDescriptor.all, src).get

  object typeRef {
    val refPat = """L([^;]+);""".r
    lazy val all: Parser[TypeRef.Public] = "B|Z|C|S|I|F|J|D|V|L[^;]+;".r ^^ {
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

  object methodDescriptor {
    lazy val all = args ~ tpe ^^ { case args ~ ret => MethodDescriptor(ret, args) }
    lazy val args = ('(' ~> rep(tpe)) <~ ')'
    lazy val tpe = typeRef.all
  }

  object fieldDescriptor {
    lazy val all = typeRef.all.map { tr => FieldDescriptor(tr) }
  }
}
