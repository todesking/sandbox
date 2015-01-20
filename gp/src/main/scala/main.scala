import com.todesking.scalagp

import scala.reflect.ClassTag

import scalagp.Ext._

object GP {
  import scalagp.{Tree, OptimizedTree, ConstLeaf, Branch2}

  val repository = new scalagp.Repository[Int]

  val random = new scala.util.Random

  val const = repository.registerConstLeaf[Int]("const",
    generateValue = () => random.nextInt(100)
  )

  val x = repository.registerFunctionLeaf[Int]("x") { ctx => ctx }

  val add2 = repository.registerBranch2[Int, Int, Int]("+2") { (c, l, r) => l(c) + r(c) }
  val sub2 = repository.registerBranch2[Int, Int, Int]("-2") { (c, l, r) => l(c) - r(c) }
  val mul2 = repository.registerBranch2[Int, Int, Int]("*2") { (c, l, r) => l(c) * r(c) }

  val add3 = repository.registerBranch3[Int, Int, Int, Int]("+3") { (ctx, a, b, c) => a(ctx) + b(ctx) + c(ctx) }
  val sub3 = repository.registerBranch3[Int, Int, Int, Int]("-3") { (ctx, a, b, c) => a(ctx) - b(ctx) - c(ctx) }
  val mul3 = repository.registerBranch3[Int, Int, Int, Int]("*3") { (ctx, a, b, c) => a(ctx) * b(ctx) * c(ctx) }

  Seq(add3, sub3, mul3) foreach { op => repository.disable(op) }

  val nashorn = repository.optimizer.newOptimizerNode[Int, String, Nashorn.Compiled]("nashorn")(src => Nashorn.Compiled(src)){case t:Nashorn.Compiled => t}

  val nashornRule = repository.optimizer.registerOptimizer("nashorn") {
    case x() =>
      nashorn("ctx")
    case const(value) =>
      nashorn(value.toString)
    case add2(nashorn(l), nashorn(r)) =>
      l + r
    case sub2(nashorn(l), nashorn(r)) =>
      l - r
    case mul2(nashorn(l), nashorn(r)) =>
      l * r
    case unk =>
      throw new RuntimeException(unk.toString)
  }

  repository.optimizer.disableOptimizer(nashornRule)

  val moveConstRule = repository.optimizer.registerOptimizer("move-const") {
    case add2(l, const(c)) =>
      add2(const(c), l)
    case sub2(l, const(c)) =>
      add2(const(-c), l)
    case mul2(l, const(c)) =>
      mul2(const(c), l)
  }
  val reducingRule = repository.optimizer.registerOptimizer("reducing", applyUntilFixed = true) {
    case add2(const(l), const(r)) =>
      const(l + r)
    case sub2(const(l), const(r)) =>
      const(l - r)
    case mul2(const(0), r) =>
      const(0)
    case mul2(const(1), r) =>
      r
    case mul2(const(l), const(r)) =>
      const(l * r)
    case mul2(const(c1), mul2(const(c2), r)) =>
      mul2(const(c1 * c2), r)
    case add2(l, r) if l == r =>
      mul2(const(2), l)
    case sub2(l, r) if l == r =>
      const(0)
    case add2(e1, mul2(e2, const(c))) if e1 == e2=>
      mul2(e2, const(c + 1))
    case add2(mul2(e2, const(c)), e1) if e1 == e2=>
      mul2(e2, const(c + 1))
    case add2(const(c1), add2(const(c2), t)) =>
      add2(const(c1 + c2), t)
    case add2(e1, sub2(e2, e3)) if e1 == e3 =>
      e2
    case sub2(add2(e1, e2), e3) if e3 == e1 =>
      e2
    case sub2(add2(e1, e2), e3) if e3 == e2 =>
      e1
    case sub2(add2(const(c1), e1), const(c2)) =>
      sub2(e1, const(c2 - c1))
  }

  object Nashorn {
    import jdk.nashorn.api.scripting.ScriptObjectMirror
    import javax.script.{ScriptEngine, ScriptEngineManager}
    import scala.collection.JavaConverters._

    lazy val engineManager = new ScriptEngineManager(null);
    lazy val engine = engineManager.getEngineByName("nashorn").asInstanceOf[ScriptEngine]

    case class Compiled(src: String) extends scalagp.Computable[Int, Int] {
      lazy val function =
        Nashorn.compile(src)
      override def apply(ctx: Int): Int =
        function.call(null, (ctx: java.lang.Integer)) match {
          case i: java.lang.Integer => i
          case d: java.lang.Double => d.toInt
        }

      def +(rhs: Compiled) =
        operator("+", rhs)
      def -(rhs: Compiled) =
        operator("-", rhs)
      def *(rhs: Compiled) =
        operator("*", rhs)

      def operator(op: String, rhs: Compiled) =
        Compiled(s"(${src}) ${op} (${rhs.src})")
    }

    def compile(src: String): ScriptObjectMirror =
      functions.get(src) getOrElse(throw new RuntimeException(s"Not found: ${src}"))

    def batchCompile(sources: Traversable[String]): Unit = {
      val uncached = sources.filterNot(functions.contains(_)).toSet.toSeq
      println(s"Compiling ${uncached.size} trees")
      val source = s"[${uncached.map(src => s"function(ctx) { return ${src} }").mkString(", ")}]"
      val compiled = engine.eval(source).asInstanceOf[javax.script.Bindings].asScala
      assert(uncached.size == compiled.size)
      for(i <- 0 until compiled.size) {
        functions.put(uncached(i), compiled(i.toString).asInstanceOf[ScriptObjectMirror])
      }
    }

    val functions = new scala.collection.mutable.HashMap[String, ScriptObjectMirror]
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    import scalagp.{Individual, Initialize, Selection, Tournament, Operation, Runner, Isle, Archipelago, World, Migrate}

    implicit val random = new scala.util.Random

    def f(x: Int) = x * x * x - 3 * x * x + 10 * x - 5

    val sampleRange = (-10 to 10)
    def score(indiv: Individual[Int, Int]): Int =
      sampleRange.foldLeft(0) { (a, x) => a + Math.pow((f(x) - indiv(x)).abs.min(1000), 2).toInt * -1 }

    val distributions = (1 to 4).map { _ =>
      GP.repository.randomDistribution(100 to 110)
    }

    val tournament = Tournament.maximizeScore[Int, Int, Int](50) { individual => score(individual) }

    val isles = distributions.map { distribution =>
      new Isle[Int, Int](
        population = 1000,
        initialize = Initialize.random(10, distribution),
        selection = Selection.default(
          tournament,
          operation = Operation.default(distribution)
        ),
        beforeSelection = { isle =>
          if(GP.repository.optimizer.optimizerEnabled(GP.nashornRule)) {
            GP.Nashorn.batchCompile(
              isle.individuals.map(_.optimized).collect { case GP.nashorn(compiled) => compiled.src }
            )
          }
        }
      )
    }

    val archipelago = new Archipelago[Int, Int](isles, Migrate.default(5, tournament))

    val runner = new Runner[Int, Int]()

    runner.run(
      archipelago,
      stop = { report => report.generation >= 100 || score(report.majolity._1) > -10 },
      describe = { individual => s"score = ${score(individual)}" }
    )
  }
}


