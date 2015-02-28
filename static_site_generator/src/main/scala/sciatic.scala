package com.todesking.sciatic

import scala.reflect.runtime.universe.TypeTag
import java.io.File
import scala.util.{Try, Success, Failure}

sealed abstract class StringValue {
}
object StringValue {
  case class Html(content: String) extends StringValue
  case class Text(content: String) extends StringValue
  def html(a: Any) = Html(a.toString)
  def text(a: Any) = Text(a.toString)
}

trait Template[A] {
  def render(param: A): String
}

sealed abstract class AST
object AST {
  case class Html(content: String) extends AST
  case class Text(content: String) extends AST
  case class ScalaCode(content: String) extends AST
  case class OutHtml(code: ScalaCode) extends AST
  case class OutText(code: ScalaCode) extends AST
  case class Nest(outer: AST, inner: AST) extends AST
  case class Multi(elements: Seq[AST]) extends AST
  val Empty = Multi(Seq())
}

trait Helpers {
  def raw(s: String) = StringValue.Html(s)
  def capture(f: Out => Unit): StringValue = {
    val out = new Out
    f(out)
    StringValue.Html(out.toString)
  }
  def render[A: TypeTag](path: String, param: A): StringValue =
    ???
}
object Helpers extends Helpers

class Out {
  def append(value: Any): Unit = value match {
    case StringValue.Text(t) => appendText(t)
    case StringValue.Html(h) => appendHtml(h)
    case x => appendText(x.toString)
  }
  def appendText(content: String): Unit = ???
  def appendHtml(content: String): Unit = ???
  override def toString(): String = ???
}

object `path/to/template` extends Template[(String, Int)] {
  override def render(param: (String, Int)): String = {
    import Helpers._
    val _out_ = new Out;

    // - val (a, b) = param
    val (a, b) = param

    // a href=a
    _out_.appendHtml("""<a href="""")
    _out_.append(a)
    _out_.appendHtml("""">""")

    //   = b
    _out_.append(b)

    _out_.appendHtml("""</a>""")

    val s = capture { _out_ =>
      _out_.append("""foo""")
      }

    _out_.append(s)

    _out_.toString
  }
}

case class ParseError(msg: String, line: Int, col: Int, longString: String) extends RuntimeException
case class CodeGenerationError() extends RuntimeException
case class CompileError() extends RuntimeException

trait Parser {
  def parse(content: String): Try[AST]
}

object Parser {
  val defaultParsers: String => Parser = Map("slim" -> new SlimParser)
}

class SlimParser extends Parser {
  override def parse(content: String): Try[AST] =
    parseLocal(content) map(translate)

  def translate(local: Seq[(Int, SlimAST)]): AST = ???

  def parseLocal(content: String): Try[Seq[(Int, SlimAST)]] =
    Definition.parseAll(Definition.all, content) match {
      case Definition.Failure(msg, rest) =>
        Failure(ParseError(msg, rest.pos.line, rest.pos.column, rest.pos.longString))
      case Definition.Error(msg, rest) =>
        Failure(ParseError(msg, rest.pos.line, rest.pos.column, rest.pos.longString))
      case Definition.Success(result, _) =>
        Success(result)
    }

  object Definition extends scala.util.parsing.combinator.RegexParsers {
    import SlimAST._

    def all: Parser[Seq[(Int, SlimAST)]] = rep(line)

    def line: Parser[(Int, SlimAST)] = indent ~ (tag | text | code | empty) <~ "$".r ^^ { case i ~ ast => (i -> ast) }

    def indent: Parser[Int] = " *".r ^^ { s => s.size }

    def identifier: Parser[String] = "[a-zA-Z0-9_][-a-zA-Z0-9_]*".r

    def tag: Parser[SlimAST] = identifier ~ rep(attributeShortcut) ~ opt(attributes) ^^ {
      case id ~ shortcuts ~ attrs => Tag(id, shortcuts.flatten.toMap ++ attrs.getOrElse(Map.empty))
    }
    def text: Parser[SlimAST] = "| " ~> ".*".r ^^ { s => Text(s) }
    def code: Parser[SlimAST] = "-|=".r ~ ".+".r ^^ {
      case "-" ~ s => Code(s)
      case "=" ~ s => Out(s)
    }
    def empty: Parser[SlimAST] = "" ^^ { _ => Empty }

    def attributeShortcut: Parser[Map[String, String]] = """\\.|#""".r ~ identifier ^^ {
      case "." ~ id => Map("class" -> CodeUtil.stringLiteral(id))
      case "#" ~ id => Map("id" -> CodeUtil.stringLiteral(id))
    }
    def attributes: Parser[Map[String, String]] = rep(identifier ~ "=" ~ simpleCode) ^^ { as =>
      as.map{ case k ~ _ ~ v => k -> v }.toMap
    }

    def simpleCode: Parser[String] = stringLiteralCode | functionCallCode | booleanLiteralCode
    def booleanLiteralCode = "true" | "false"
    def stringLiteralCode = "\"" ~ "([^\"]|\\\\\")*".r ~ "\"" ^^ { case a ~ b ~ c => a + b + c }
    def functionCallCode = "[a-zA-Z0-9+]+".r ~ "(" ~ repsep(simpleCode, ",") ~ ")" ^^ { case a ~ b ~ c ~ d => a + b + c + d }
  }
}

sealed abstract class SlimAST
object SlimAST {
  case class Code(code: String) extends SlimAST
  case class Out(code: String) extends SlimAST
  case class Tag(name: String, attributes: Map[String, String]) extends SlimAST
  case class Text(content: String) extends SlimAST
  case object Empty extends SlimAST
}

case class TemplateCode[A](content: String)

object CodeUtil {
  def stringLiteral(s: String) =
    s""""${s.replace("\\\\", "\\\\").replace("\"", "\\\"")}""""
}

class CodeGenerator {
  def generate[A: TypeTag](packageName: String, className: String, ast: AST): TemplateCode[A] = {
    val paramClass = implicitly[TypeTag[A]]
    val paramClassName = paramClass.toString
    val whole = s"""
      |package ${packageName}
      |final class ${className} extends Template[${paramClassName}] {
      |  override def render(param: ${paramClassName}): String = {
      |${generateBody(ast).mkString(";\n")}
      |  }
      |}
    """.stripMargin.trim
    TemplateCode(whole)
  }
  def generateBody(ast: AST): Seq[String] = {
    import AST._
    ast match {
      case Html(content) =>
        Seq(s"_out_.appendHtml(${string(content)})")
      case Text(content) =>
        Seq(s"_out_.appendText(${string(content)})")
      case ScalaCode(code) =>
        Seq(code)
      case OutHtml(ScalaCode(code)) =>
        Seq(s"_out_.appendHtml(${code})")
      case OutText(ScalaCode(code)) =>
        Seq(s"_out_.appendText(${code})")
      case Nest(outer, inner) =>
        generateBody(outer) ++ generateBody(inner)
      case Multi(elements) =>
        elements flatMap(generateBody(_))
    }
  }
  def string(content: String): String = {
    def escape(s: String) = s.replace("\\", "\\\\").replace("\"", "\\\"")
    val q = "\""
    s"""${q}${escape(content)}${q}"""
  }
}

class Compiler(sourceDir: File, bytecodeDir: File) {
  def compile[Ctx: TypeTag](code: TemplateCode[Ctx]): Try[CompiledTemplate[Ctx]] = ???
}

trait TemplateRepository {
  def load[A: TypeTag](path: String): Template[A]
  def paths(): Seq[String]
}

class CompiledTemplate[A: TypeTag](path: String, timestamp: Long) extends Template[A] {
  def cast[B: TypeTag](): CompiledTemplate[B] =
    if(implicitly[TypeTag[A]] == implicitly[TypeTag[B]])
      this.asInstanceOf[CompiledTemplate[B]]
    else
      throw new IllegalStateException(
        s"Cast failed: CompiledTemplate[${implicitly[TypeTag[B]]}] required, but actual type is CompiledTemplate[${implicitly[TypeTag[A]]}]")

  override def render(param: A) = ???
}

class TemplateDirectory(root: File) {
  def timestamp(path: String): Option[Long] = ???
  def read(path: String): Try[TemplateSource] = ???
  def paths(): Seq[String] = ???
}

case class TemplateSource(format: String, content: String)

class LiveTemplateRepository(
  packageName: String,
  templateDirectory: TemplateDirectory,
  parsers: String => Option[Parser],
  codeGenerator: CodeGenerator,
  compiler: Compiler
) extends TemplateRepository {
  private[this] var compiledTemplates: Map[String, (Long, CompiledTemplate[_])] = Map.empty

  override def load[A: TypeTag](path: String): LiveTemplate[A] = {
    reload(path)
    new LiveTemplate[A](path, this)
  }

  override def paths(): Seq[String] = templateDirectory.paths()

  def reload[A: TypeTag](path: String): Try[CompiledTemplate[A]] =
    (for {
      (timestamp, template) <- compiledTemplates.get(path) if isFresh(path, timestamp)
    } yield {
      Success(template.cast[A])
    }) getOrElse {
      forceReload[A](path)
    }

  private[this] def isFresh(path: String, timestamp: Long): Boolean =
    templateDirectory.timestamp(path) match {
      case None => false
      case Some(ts) => timestamp >= ts
    }

  def forceReload[A: TypeTag](path: String): Try[CompiledTemplate[A]] = synchronized {
    for {
      source <- templateDirectory.read(path)
      parser <- parsers(source.format).map(Success(_)).getOrElse(Failure(new RuntimeException(s"Unknown format: ${source.format}")))
      ast <- parser.parse(source.content)
      code = codeGenerator.generate[A](packageName, path, ast)
      compiled <- compiler.compile(code)
    } yield {
      compiled
    }
  }
}

class LiveTemplate[A: TypeTag](path: String, repo: LiveTemplateRepository) extends Template[A] {
  override def render(param: A): String =
    tryRender(param).get

  def tryRender(param: A): Try[String] =
    repo.reload[A](path).map(_.render(param))
}
