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

  repository.optimizeRule {
    case add(add(a, b), add(c, d)) =>
      add_*(Seq(a, b, c, d))
    case add(add(a, b), c) =>
      add_*(Seq(a, b, c))
    case add(a, add(b, c)) =>
      add_*(Seq(a, b, c))
    case add(add_*(da), add_*(db)) =>
      add_*(da ++ db)
    case add(add_*(da), b) =>
      add_*(da :+ b)
    case add(a, add_*(db)) =>
      add_*(a +: db)
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


