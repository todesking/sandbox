import com.todesking.scalagp

import scala.reflect.ClassTag

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

  val nashorn = repository.registerOptimizerNode[Int, Nashorn.Compiled]("nashorn") { (ctx, compiled) => compiled(ctx) }

  val nashornRule = repository.registerOptimizer("nashorn") {
    case x() =>
      nashorn(Nashorn.Compiled("ctx"))
    case const(value) =>
      nashorn(Nashorn.Compiled(value.toString))
    case add2(nashorn(l), nashorn(r)) =>
      nashorn(l + r)
    case sub2(nashorn(l), nashorn(r)) =>
      nashorn(l - r)
    case mul2(nashorn(l), nashorn(r)) =>
      nashorn(l * r)
    case unk =>
      throw new RuntimeException(unk.toString)
  }

  repository.disableOptimizer(nashornRule)

  object Nashorn {
    import jdk.nashorn.api.scripting.ScriptObjectMirror
    import javax.script.{ScriptEngine, ScriptEngineManager}
    import scala.collection.JavaConverters._

    lazy val engineManager = new ScriptEngineManager(null);
    lazy val engine = engineManager.getEngineByName("nashorn").asInstanceOf[ScriptEngine]

    case class Compiled(src: String) {
      lazy val function =
        Nashorn.compile(src)
      def apply(ctx: Int): Int =
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
    import scalagp.{Individual, Initialize, Selection, Tournament, Runner}

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
      ),
      beforeSelection = { isle =>
        if(GP.repository.optimizerEnabled(GP.nashornRule)) {
          GP.Nashorn.batchCompile(
            isle.individuals.map(_.optimized).collect { case GP.nashorn(compiled) => compiled.src }
          )
        }
      }
    )

    val runner = new Runner[Int, Int]()

    runner.run(
      isle,
      stop = { report => report.generation >= 100 || score(report.majolity._1) > -10 },
      describe = { individual => s"score = ${score(individual)}" }
    )
  }
}


