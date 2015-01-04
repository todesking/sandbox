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

  val add_* = repository.registerOptimized[Int, Seq[Tree[Int, Int]]]("add*") { (ctx, children) =>
    children.foldLeft(0) { (a, c) => a + c(ctx) }
  }
  val mul_* = repository.registerOptimized[Int, Seq[Tree[Int, Int]]]("mul*") { (ctx, children) =>
    children.foldLeft(1) { (a, c) => a * c(ctx) }
  }

  def b2Fusion[A: Class](o: scalagp.Branch2Definition[A, Int, A, A, scalagp.Branch2[A, Int, A, A]], p: scalagp.OptimizeDefinition[A, Int, Seq[Tree[A, Int]]]): Unit =
    repository.optimizeRule[A, Seq[Tree[A, Int]]] {
      case o(o(a, b), o(c, d)) =>
        p(Seq(a, b, c, d))
      case o(o(a, b), c) =>
        p(Seq(a, b, c))
      case o(a, o(b, c)) =>
        p(Seq(a, b, c))
      case o(p(da), p(db)) =>
        p(da ++ db)
      case o(p(da), b) =>
        p(da :+ b)
      case o(a, p(db)) =>
        p(a +: db)
    }

  b2Fusion(add, add_*)
  b2Fusion(mul, mul_*)
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


