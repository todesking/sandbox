package com.todesking.fizlang
import scala.util.parsing.combinator.RegexParsers

sealed abstract class Expr
object Expr {
  case class Lit(value: Any) extends Expr
  case class IntPlus(l: Expr, r: Expr) extends Expr
  case class IntMinus(l: Expr, r: Expr) extends Expr
  case class IntMod(l: Expr, r: Expr) extends Expr
  case class If(cond: Expr, th: Expr, el: Expr) extends Expr
  case class Fun(param: String, body: Expr) extends Expr
  case class Ref(param: String) extends Expr
  case class App(fun: Expr, arg: Expr) extends Expr
}

object Parser extends RegexParsers {
  def parseExpr(src: String): Either[String, Expr] =
    translate(parseAll(expr, src))
  def parseToplevel(src: String): Either[String, Seq[(String, Expr)]] =
    translate(parseAll(toplevel, src))

  private[this] def translate[A](a: ParseResult[A]): Either[String, A] =
    a match {
      case Success(e, _)    => Right(e)
      case NoSuccess(e, in) => Left(e + "\n" + in.pos.longString)
    }

  val keywords = Set(
    "fun",
    "if",
    "then",
    "else"
  )

  val E = Expr

  def toplevel: Parser[Seq[(String, Expr)]] = term.+

  def term = top_let

  def top_let = ("let" ~> name) ~ name.* ~ ("=" ~> expr) <~ ";" ^^ {
    case name ~ params ~ body =>
      name -> params.foldRight(body) {
        case (p, e) =>
          E.Fun(p, e)
      }
  }

  def expr: Parser[Expr] = op | app | fun | ifelse | expr1
  def expr1: Parser[Expr] = paren | lit_int | lit_str | ref

  def lit_int = "[0-9]+".r ^^ { x =>
    E.Lit(x.toInt)
  }
  def lit_str = ("\"" ~> """[^"]*""".r) <~ "\"" ^^ { x =>
    E.Lit(x)
  }
  def ref = name ^^ { x =>
    E.Ref(x)
  }
  def fun = ("fun" ~> name) ~ ("=>" ~> expr) ^^ {
    case param ~ body => E.Fun(param, body)
  }
  def name = "[a-z]+".r ^? { case n if !keywords.contains(n) => n }
  def app = expr1 ~ expr1.+ ^^ {
    case l ~ rs =>
      rs.foldLeft(l) { case (l, r) => E.App(l, r) }
  }
  def ifelse = ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ {
    case cond ~ th ~ el => E.If(cond, th, el)
  }
  def op = expr1 ~ "[-+%]".r ~ expr1 ^^ {
    case lhs ~ "+" ~ rhs => E.IntPlus(lhs, rhs)
    case lhs ~ "-" ~ rhs => E.IntMinus(lhs, rhs)
    case lhs ~ "%" ~ rhs => E.IntMod(lhs, rhs)
  }
  def paren = ("(" ~> expr) <~ ")"
}

object Interpreter {
  val E = Expr
  type Env = Map[String, Any]
  class Error(msg: String) extends RuntimeException(msg)

  case class FunData(param: String, body: Expr, env: Env)
  case class Instinct(name: String, f: Any => Any)

  val defaultEnv = Seq(
    "println" -> { x: Any =>
      println(x)
    }
  ).map { case (k, v) => k -> Instinct(k, v) }.toMap ++ Map(
    "true" -> true,
    "false" -> false
  )

  def runExpr(src: String, env: Env = defaultEnv): Any = {
    Parser.parseExpr(src) match {
      case Left(msg)   => throw new Error("Parse error: " + msg)
      case Right(expr) => eval(expr, env)
    }
  }
  def runScript(src: String, env: Env = defaultEnv): Env =
    Parser.parseToplevel(src) match {
      case Left(msg) => throw new Error(s"Parse error: $msg")
      case Right(xs) =>
        xs.foldLeft(env) {
          case (env, (name, expr)) =>
            val v = eval(expr, env)
            env + (name -> v)
        }
    }
  def runMain(src: String, env: Env = defaultEnv): Any = {
    val e = runScript(src, env)
    val main = evalFun(E.Ref("main"), e)
    eval(main.body, e + (main.param -> ()))
  }

  def eval(expr: Expr, env: Env = defaultEnv): Any = expr match {
    case E.Lit(value) => value
    case E.IntPlus(l, r) =>
      evalInt(l, env) + evalInt(r, env)
    case E.IntMinus(l, r) =>
      evalInt(l, env) - evalInt(r, env)
    case E.IntMod(l, r) =>
      evalInt(l, env) % evalInt(r, env)
    case E.If(cond, th, el) =>
      if (evalBool(cond, env)) eval(th, env)
      else eval(el, env)
    case E.App(f, a) =>
      val fun = eval(f, env)
      val arg = eval(a, env)
      fun match {
        case FunData(param, body, env) =>
          eval(body, env + (param -> arg))
        case Instinct(_, f) =>
          f(arg)
      }
    case E.Fun(param, body) =>
      FunData(param, body, env)
    case E.Ref(name) =>
      env.get(name).getOrElse {
        throw new Error(s"Name not found: $name")
      }
  }
  def evalInt(expr: Expr, env: Env = defaultEnv): Int =
    eval(expr, env) match {
      case x: Int => x
      case unk    => throw new Error(s"Expected int: $unk")
    }
  def evalBool(expr: Expr, env: Env = defaultEnv): Boolean =
    eval(expr, env) match {
      case x: Boolean => x
      case unk        => throw new Error(s"Expected bool: $unk")
    }
  def evalFun(expr: Expr, env: Env = defaultEnv): FunData =
    eval(expr, env) match {
      case f: FunData => f
      case unk        => throw new Error(s"Expected fun: $unk")
    }
}

object Main {
  def test(s: String): Unit = {
    println(s)
    val result = try {
      Interpreter.runExpr(s).toString
    } catch {
      case e: Interpreter.Error => "[Error] " + e.getMessage
    }
    println(s"=> $result")
  }
  def testScript(s: String): Unit = {
    println(s)
    val result = try {
      Interpreter.runMain(s)
    } catch {
      case e: Interpreter.Error => "[Error] " + e.getMessage
    }
    println(s"=> $result")
  }
  def main(args: Array[String]): Unit = {
    test("1")
    test(""""Hello world!"""")
    test("1 + 2")
    test("(fun x => x + 1) 10")
    test("println(1 + 2)")
    test("if true then 1 else 2")
    test("if false then 1 else 2")
    testScript("""
      let a = 1;
      let b = 2;
      let add x y = x + y ;
      let main x = add a b ;
    """)
  }
}
