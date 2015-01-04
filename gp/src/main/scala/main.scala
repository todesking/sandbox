import com.todesking.scalagp

object GP {
  val repository = new scalagp.Repository[Int]

  val random = new scala.util.Random

  object Tree {
    import repository._
    import scalagp.implicits.BasicClasses._

    val const = registerConstLeaf[Int]("const",
      generateValue = () => random.nextInt(100)
    )

    val x = registerLeaf[Int]("x") { ctx => ctx }

    val add = registerBranch2[Int, Int, Int]("+") { case (ctx, (l, r)) => l(ctx) + r(ctx) }
    val sub = registerBranch2[Int, Int, Int]("-") { case (ctx, (l, r)) => l(ctx) - r(ctx) }
    val mul = registerBranch2[Int, Int, Int]("*") { case (ctx, (l, r)) => l(ctx) * r(ctx) }
  }
  Tree // force initialize
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


