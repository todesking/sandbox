import com.todesking.scalagp

object GP {
  import scalagp.{Tree, OptimizedTree}

  val repository = new scalagp.Repository[Int]

  val random = new scala.util.Random

  import scalagp.implicits.BasicClasses._

  val const = repository.registerConstLeaf[Int]("const",
    generateValue = () => random.nextInt(100)
  )

  val x = repository.registerLeaf[Int]("x") { ctx => ctx }

  val add = repository.registerBranch2[Int, Int, Int]("+") { (c, l, r) => l(c) + r(c) }
  val sub = repository.registerBranch2[Int, Int, Int]("-") { (c, l, r) => l(c) - r(c) }
  val mul = repository.registerBranch2[Int, Int, Int]("*") { (c, l, r) => l(c) * r(c) }


  object Nashorn {
    import javax.script.{ScriptEngine, ScriptEngineManager}
    import jdk.nashorn.api.scripting.ScriptObjectMirror

    val engine = new ScriptEngineManager(null).getEngineByName("nashorn").asInstanceOf[ScriptEngine]

    val cache =
      new scala.collection.mutable.HashMap[String, ScriptObjectMirror]

    def toFunction(body: String): ScriptObjectMirror = {
      cache.get(body) getOrElse {
        println(s"Compiling ${body}")
        val value = engine.eval(s"function(ctx) { return ${body} }").asInstanceOf[ScriptObjectMirror]
        cache.put(body, value)
        value
      }
    }

    case class Compiled(body: String) {
      lazy val func =
        toFunction(body)

      def apply(ctx: Int): Int =
        func.call(null, ctx.asInstanceOf[java.lang.Object]) match {
          case i: java.lang.Integer => i
          case d: java.lang.Double => d.toInt
        }
    }
  }

  val jsCompiled = repository.registerOptimized[Int, Nashorn.Compiled]("<JS>") { (ctx, compiled) => compiled(ctx) }

  repository.optimizeRule {
    case const(a) =>
      jsCompiled(Nashorn.Compiled(a.toString))
    case x() =>
      jsCompiled(Nashorn.Compiled("ctx"))
    case add(jsCompiled(l), jsCompiled(r)) =>
      jsCompiled(Nashorn.Compiled(s"(${l.body}) + (${r.body})"))
    case sub(jsCompiled(l), jsCompiled(r)) =>
      jsCompiled(Nashorn.Compiled(s"(${l.body}) - (${r.body})"))
    case mul(jsCompiled(l), jsCompiled(r)) =>
      jsCompiled(Nashorn.Compiled(s"(${l.body}) * (${r.body})"))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    import scalagp.{Individual, Initialize, Selection, Tournament, Runner}
    import scalagp.implicits.BasicClasses._

    implicit val random = new scala.util.Random

    def f(x: Int) = x * x * x - 3 * x * x + 10 * x - 5

    val sampleRange = (-10 to 10)
    def score(indiv: Individual[Int, Int]): Int =
      sampleRange.foldLeft(0) { (a, x) => a + Math.pow((f(x) - indiv(x)).abs.min(1000), 2).toInt * -1 }

    val isle = new scalagp.Isle[Int, Int](
      repository = GP.repository,
      population = 1000,
      initialize = Initialize.random(20),
      selection = Selection.default(
        Tournament.maximizeScore(50) { individual => score(individual) }
      )
    )

    val runner = new Runner[Int, Int]()

    runner.run(
      isle,
      stop = { report => report.generation >= 100 || score(report.majolity._1) > -10 },
      describe = { individual => s"score = ${score(individual)}" }
    )
  }
}


