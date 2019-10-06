package com.todesking.fizlang
import scala.util.parsing.combinator.RegexParsers

sealed abstract class Expr
object Expr {
  case class Lit(value: Any) extends Expr
  case class IntPlus(l: Expr, r: Expr) extends Expr
  case class IntMinus(l: Expr, r: Expr) extends Expr
  case class IntMul(l: Expr, r: Expr) extends Expr
  case class IntDiv(l: Expr, r: Expr) extends Expr
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

  private[this] def repseps1[A, B](
      x: Parser[A],
      sep: Parser[B]
  ): Parser[A ~ Seq[(B, A)]] = {
    x ~ (sep ~ x).* ^^ {
      case x ~ xs =>
        new ~(x, xs.map { case b ~ a => (b, a) })
    }
  }
  private[this] def handleOp(
      f: PartialFunction[(Expr, String, Expr), Expr]
  ): (Expr ~ Seq[(String, Expr)]) => Expr = {
    case x ~ xs =>
      xs.foldLeft(x) {
        case (l, (op, r)) =>
          f((l, op, r))
      }
  }

  def expr: Parser[Expr] = fun | ifelse | expr1
  def name = "[a-z][a-z_]*".r ^? { case n if !keywords.contains(n) => n }
  def fun = ("fun" ~> name) ~ ("=>" ~> expr) ^^ {
    case param ~ body => E.Fun(param, body)
  }
  def ifelse = ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ {
    case cond ~ th ~ el => E.If(cond, th, el)
  }
  def expr1: Parser[Expr] = repseps1(expr2, "$") ^^ {
    case x ~ xs =>
      xs.foldLeft(x) {
        case (l, (op, r)) =>
          E.App(l, r)
      }
  }
  def expr2: Parser[Expr] = repseps1(expr3, "[-+]".r) ^^ handleOp {
    case (l, "+", r) =>
      E.IntPlus(l, r)
    case (l, "-", r) =>
      E.IntMinus(l, r)
  }
  def expr3: Parser[Expr] = repseps1(expr4, "[*/%]".r) ^^ handleOp {
    case (l, "*", r) =>
      E.IntMul(l, r)
    case (l, "/", r) =>
      E.IntDiv(l, r)
    case (l, "%", r) =>
      E.IntMod(l, r)
  }
  def expr4: Parser[Expr] = repseps1(expr5, ".") ^^ handleOp {
    case (l, ".", r) =>
      E.Fun("$x", E.App(l, E.App(r, E.Ref("$x"))))
  }
  def expr5: Parser[Expr] = expr6 ~ rep(expr6) ^^ {
    case x ~ xs =>
      xs.foldLeft(x) { case (l, r) => E.App(l, r) }
  }
  def expr6: Parser[Expr] = paren | lit_int | lit_str | ref

  def paren = ("(" ~> expr) <~ ")"
  def lit_int = "[0-9]+".r ^^ { x =>
    E.Lit(x.toInt)
  }
  def lit_str = ("\"" ~> """[^"]*""".r) <~ "\"" ^^ { x =>
    E.Lit(x)
  }
  def ref = name ^^ { x =>
    E.Ref(x)
  }
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
    },
    "char_to_int" -> { x: Any =>
      x.asInstanceOf[Char].toInt
    },
    "int_to_char" -> { x: Any =>
      x.asInstanceOf[Int].toChar
    },
    "char_to_string" -> { x: Any =>
      x.asInstanceOf[Char].toString
    },
    "string_concat" -> { x: Any =>
      Instinct("string_concat_1", { y: Any =>
        x.asInstanceOf[String] + y.asInstanceOf[String]
      })
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
    case E.IntMul(l, r) =>
      evalInt(l, env) * evalInt(r, env)
    case E.IntDiv(l, r) =>
      evalInt(l, env) / evalInt(r, env)
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
    println(s"> $s")
    val result = try {
      Interpreter.runExpr(s).toString
    } catch {
      case e: Interpreter.Error => "[Error] " + e.getMessage
    }
    println(s"=> $result")
  }
  def testScript(s: String): Unit = {
    println(s"> $s")
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
    test("1 * 2")
    test("1 / 2")
    test("1 % 2")
    test("(fun x => x + 1) 10")
    test("println(1 + 2)")
    test("if true then 1 else 2")
    test("if false then 1 else 2")
    test("println . char_to_string . int_to_char $ 42")
    testScript("""
      let a = 1;
      let b = 2;
      let add x y = x + y ;
      let main x = add a b ;
    """)

    testScript("""
      let main x =
        foreach 1 30 $ print . fizzbuzz_str

      let fizzbuzz_str n = if
          | n % 15 == 0 => "FizzBuzz"
          | n % 3 == 0 => "Fizz"
          | n % 5 == 0 => "Buzz"
          | else => num_to_str n ;

      let foreach from to f = if
          | from <= to => { f from; foreach $ from + 1 $ to }
          | else => ()
          ;

      let num_to_str n =
          let last_digit n =
              char_to_string $ int_to_char $ (char_to_int '0') + (n % 10) in
          let impl n s =
              if n < 10 then last_digit n
              else impl $ n / 10 $ (string_concat $ last_digit n $ s) in
          impl n "" ;
    """)
  }
}
