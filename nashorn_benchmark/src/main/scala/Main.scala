object Main {
  def bench(name: String, n: Int)(f: Int => Unit): Unit = {
    val start = System.nanoTime()
    (0 to n) foreach { i => f(i) }
    println(s"${name}: ${(System.nanoTime - start) / 1000 / 1000}[ms]")
  }

  def main(args: Array[String]): Unit = {
    val s = S.Parser.parse("(+ x 1 2 3 (+ 4 (+ 5 x) 6 x))")
    val env = S.Env.global

    val N = 1000 * 30 * 20 * 20

    import javax.script.{ScriptEngine, ScriptEngineManager, Invocable}

    val engineManager = new ScriptEngineManager(null);
    val engine = engineManager.getEngineByExtension("js").asInstanceOf[ScriptEngine with Invocable]
    engine.eval("function f(x) { return (x + 1 + 2 + 3 + (4 + (5 + x) + 6 + x)); }");

    bench(s"${engine.getFactory().getEngineName()} compile", 1000 * 100) { i =>
      engine.eval(s"function(ctx) { return ${Data.veryLongSource} }");
    }

    (0 to 5) foreach { _ =>
      bench("Neive", N) { i =>
        env.put(S.Sym("x"), S.Atom(i))
        s.eval(env)
      }

      bench(engine.getFactory().getEngineName(), N) { i =>
        engine.invokeFunction("f", i: java.lang.Integer)
      }
    }
  }
}

object Data {
  val veryLongSource = """(((((29) - (16)) * ((((ctx) + ((ctx) - ((ctx) + ((34) * (15))))) + (((ctx) + (42)) + (42))) + ((ctx) - (((ctx) - (((ctx) - (48)) + (ctx))) * ((ctx) * (97)))))) * (((((((ctx) * (ctx)) - ((0) - ((ctx) * (6)))) * ((90) * (ctx))) * ((39) - (ctx))) * ((54) + (((((79) + (((22) + ((((ctx) - (70)) * (87)) - (28))) * ((97) * (((((ctx) - (6)) * (31)) + (((((ctx) - ((93) - ((ctx) - (54)))) + (((61) - (ctx)) + (((ctx) * (47)) - ((81) * (38))))) + ((31) * ((ctx) + (((ctx) - (45)) - (ctx))))) + (74))) + ((51) - (((79) + ((60) - ((ctx) - (((ctx) * (ctx)) - ((68) + (50)))))) - ((21) * ((ctx) - ((((56) * (ctx)) * ((ctx) - (ctx))) - (48)))))))))) * (((ctx) + ((ctx) - (22))) + (72))) * (25)) * ((26) * ((((ctx) + (27)) + (20)) * (47)))))) + (ctx))) + (((53) - ((((((ctx) - (55)) * (ctx)) - ((8) * ((ctx) + ((69) - ((ctx) + ((15) * (ctx))))))) + (ctx)) + (ctx))) + (((((((80) * ((94) * ((ctx) - ((18) - (((((((((ctx) * (ctx)) * (2)) - ((67) * (67))) + (((ctx) - ((ctx) + (ctx))) + (((ctx) + (26)) * ((ctx) + (21))))) - (((((ctx) - (57)) + (42)) * ((ctx) * (ctx))) - ((ctx) - (((ctx) - (53)) - ((ctx) - (41)))))) - ((((((ctx) - (81)) - (14)) * (43)) + ((43) + ((ctx) + (37)))) + (ctx))) * (((ctx) - (95)) * (4))) * (63)))))) * (ctx)) * (((ctx) * ((82) + ((((((((((ctx) + ((59) + (ctx))) - ((2) + ((95) - (ctx)))) + (ctx)) * ((((64) * (70)) + (ctx)) + (30))) * ((ctx) * ((10) - ((ctx) * ((ctx) + ((ctx) * (7))))))) + (((((((ctx) * (ctx)) + ((ctx) * (24))) + (80)) * (ctx)) - (((((80) + (ctx)) * ((85) + (75))) - (((68) + (6)) + ((4) + (ctx)))) - ((((11) * (ctx)) - (54)) + ((56) * ((2) * (3)))))) + (ctx))) * (((((ctx) * (ctx)) * (((((41) - (52)) + ((ctx) - (52))) * (ctx)) + ((((91) - (92)) + ((78) * (ctx))) - (((84) + (ctx)) + (ctx))))) - ((((ctx) + (93)) - (ctx)) * (((ctx) - ((ctx) * (65))) + (82)))) + (81))) + ((45) + (((((((52) + ((97) * (ctx))) * (70)) - (ctx)) * (ctx)) - ((((((ctx) - (15)) + ((ctx) * (ctx))) * (ctx)) * (20)) - (31))) * (((((42) - ((ctx) - (27))) * (41)) + ((((ctx) * ((43) + (ctx))) + (ctx)) + (ctx))) + ((((((ctx) * (ctx)) * ((31) + (ctx))) + (((69) - (14)) - ((33) * (ctx)))) - (ctx)) + ((84) * ((((37) + (ctx)) + (45)) + (((66) + (41)) - ((54) * (ctx)))))))))) + (ctx)))) - (10))) + (ctx)) - (((ctx) + (((42) - (((((31) + (27)) + (ctx)) - (((((((ctx) + (85)) + (ctx)) + (((62) + ((54) - (((13) - (ctx)) * ((ctx) * (65))))) - ((29) - ((ctx) * ((ctx) - (ctx)))))) - ((93) + (((ctx) + ((91) - (ctx))) - ((((ctx) + (ctx)) + ((1) * ((24) - (ctx)))) * (ctx))))) + ((28) * (93))) * (((((5) + (ctx)) * ((((71) + (83)) * ((((ctx) * (60)) * ((39) - (ctx))) + (ctx))) * (16))) * (30)) * (ctx)))) - (ctx))) + (((((ctx) - ((49) * ((96) * ((ctx) * ((38) + (((((ctx) - (ctx)) + ((ctx) + (ctx))) - (ctx)) + ((56) + ((69) + (43))))))))) - (ctx)) * (31)) * (43)))) * ((((((60) + (87)) + (47)) * (((48) - (ctx)) - ((((((69) + (((37) - (((41) + ((ctx) * (88))) + (33))) + ((ctx) * (ctx)))) * ((65) + ((ctx) - ((((ctx) - ((41) - (ctx))) - (((94) - (77)) + (47))) + (ctx))))) * (((ctx) + (96)) - (((2) * (((ctx) * ((ctx) * (ctx))) + ((3) - ((ctx) * (12))))) * (((19) - ((((ctx) - (ctx)) * ((ctx) - (ctx))) * ((76) * ((78) * (22))))) + (91))))) * (51)) + (((ctx) - (ctx)) - (ctx))))) - (((ctx) * ((82) + ((((((5) + ((ctx) - ((42) - ((ctx) - ((93) + (ctx)))))) - (46)) * (ctx)) - ((ctx) - ((18) * ((((((27) - (ctx)) - ((ctx) * (ctx))) - (95)) * ((((24) * (18)) * ((ctx) * (44))) - (((97) * (ctx)) - ((ctx) - (ctx))))) * (((7) + ((ctx) - (ctx))) + ((((ctx) + (36)) - (69)) + (((93) - (44)) - (96)))))))) + ((58) - (10))))) * ((((ctx) + (((43) * (53)) * (ctx))) + (((ctx) * (((49) * (13)) - (38))) + ((((((86) + (ctx)) - ((ctx) * ((30) - (((ctx) - (23)) * (88))))) * (((ctx) + (34)) * ((20) + ((77) * (68))))) - ((22) * (21))) - ((21) - (37))))) - ((ctx) - ((((91) + ((ctx) - ((((67) * ((ctx) * ((ctx) - (90)))) - ((18) * (61))) * ((((ctx) + ((ctx) * (ctx))) + (((ctx) + (ctx)) * (65))) - (76))))) * (11)) * (ctx)))))) * (78)))) * (9)))) + (81)"""
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
