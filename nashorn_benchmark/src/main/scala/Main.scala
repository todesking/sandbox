object Main {
  def bench(name: String, n: Int)(f: Int => Unit): Unit = {
    val start = System.nanoTime()
    (0 to n) foreach { i => f(i) }
    println(s"${name}(N=$n): ${(System.nanoTime - start) / 1000 / 1000}[ms]")
  }

  def main(args: Array[String]): Unit = {
    def native(x: Int) =
      x + 1 + 2 + 3 + (4 + (5 + x) + 6 + x)

    val s = S.Parser.parse("(+ x 1 2 3 (+ 4 (+ 5 x) 6 x))")
    val env = S.Env.global

    val N = 1000 * 30 * 20 * 20

    import javax.script.{ScriptEngine, ScriptEngineManager, Invocable}

    val engineManager = new ScriptEngineManager(null);
    val engine = engineManager.getEngineByExtension("js").asInstanceOf[ScriptEngine with Invocable]

    val compiled = engine.eval("function(x) { return (x + 1 + 2 + 3 + (4 + (5 + x) + 6 + x)); }")
      .asInstanceOf[jdk.nashorn.api.scripting.ScriptObjectMirror]

      engine.eval("function add(args) { var i = 0, l = args.length, sum = 0; for(; i < l; i++) sum += args[i]; return sum }")
    val compiledFun = engine.eval("function(x) { return add(x, 1, 2, 3, add(4, add(5, x), 6, x)); }")
      .asInstanceOf[jdk.nashorn.api.scripting.ScriptObjectMirror]

    (1 to 3) foreach { _ =>
      bench("Native", N) { i =>
        native(i)
      }

      bench("Neive", N) { i =>
        env.put(S.Sym("x"), S.Atom(i))
        s.eval(env)
      }

      bench(s"${engine.getFactory().getEngineName()}(+)", N) { i =>
        compiled.call(null, i: java.lang.Integer)
      }

      bench(s"${engine.getFactory().getEngineName()}(function)", N) { i =>
        compiledFun.call(null, i: java.lang.Integer)
      }

      bench(s"${engine.getFactory().getEngineName()} define function(cached)", 1000) { i =>
        engine.eval(s"function(x) { return x + 1 }")
      }
      bench(s"${engine.getFactory().getEngineName()} define function(uncached)", 1000) { i =>
        engine.eval(s"function(x) { return x + ${i} }")
      }
      bench(s"${engine.getFactory().getEngineName()} define function(uncached, batch x10)", 100) { i =>
        engine.eval(
          (0 to 10) map { j => s"function f_${i}_${j}(x) { return ${i} + ${j} };" } mkString("")
        )
      }
      bench(s"${engine.getFactory().getEngineName()} define function(uncached, batch x10)", 1) { i =>
        engine.eval(
          (0 to 1000) map { j => s"function f_${i}_${j}(x) { return ${i} + ${j} };" } mkString("")
        )
      }
    }
  }
}

package S {
  import scala.util.parsing.combinator._

  object Parser extends RegexParsers {
    def expr: Parser[Expr] = atom | list

    def atom = intNum | symbol
    def list = "(" ~> rep1(expr, expr) <~ ")" ^^ { list => SList(list) }

    def intNum = """[1-9][0-9]*""".r ^^ { s => Atom(s.toInt) }
    def symbol = """[a-z+*/^!?\-][a-z+*/^!?0-9\-]*""".r ^^ { s => Sym(s) }

    def parse(source: String): Expr =
      parseAll(expr, source) match {
        case Success(expr, _) =>
          expr
        case failure: NoSuccess =>
          throw new RuntimeException(s"Parse error: msg=${failure.msg}, source=${source}")
      }
  }

  class Env {
    val storage = new scala.collection.mutable.HashMap[Sym, Expr]

    def get(sym: Sym): Expr =
      storage(sym)

    def put(sym: Sym, value: Expr) =
      storage.put(sym, value)
  }

  sealed abstract class Expr {
    def eval(env: Env = Env.global): Expr =
      this
    def toInt: Int =
      throw new RuntimeException("not a number")
  }
  object Env {
    val global = new Env

    global.put(Sym("+"), Lambda(global, { (env, args) => Atom(args.foldLeft(0) {(a, x) => a + x.eval(env).toInt}) }))
  }
  case class Sym(value: String) extends Expr {
    override def toString = value
    override def eval(env: Env): Expr =
      env.get(this)
  }
  case class Atom(value: Any) extends Expr {
    override def toString = value.toString
    override def toInt =
      value match { case i: Int => i; case _ => super.toInt }
  }
  case class Lambda(env: Env, fun: (Env, Seq[Expr]) => Expr) extends Expr {
    override def toString = "<lambda>"
  }
  case class SList(ars: Seq[Expr]) extends Expr {
    override def toString = s"(${ars.mkString(" ")})"
    override def eval(env: Env): Expr =
      ars.head.eval(env) match {
        case Lambda(env, fun) =>
          fun(env, ars.tail)
        case _ =>
          throw new RuntimeException()
      }
  }
}
