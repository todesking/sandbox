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
    import scalagp.{Individual, Initialize, Selection, Tournament}
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
        Tournament.maximizeScore(30) { individual => score(individual) }
      )
    )

    println("Definitions:")
    GP.repository.allDefinitions.foreach { d =>
      println(s"  * ${d}")
    }

    var report = isle.nextGeneration()
    while(isle.generation < 1000) {
      println(s"Generation ${isle.generation}")
      val sampled = isle.individuals.head
      println(s"sample: score=${score(sampled)}")
      println(sampled.tree.toString)
      println(s"Uniqueness: ${report.uniqueIndividuals}/${report.individuals.size}")
      println(s"Size(99, 90, 50): ${report.percentiles(99, 90, 50)(_.tree.size).map(_._1).mkString(", ")}")
      println(s"Depth(99, 90, 50): ${report.percentiles(99, 90, 50)(_.tree.height).map(_._1).mkString(", ")}")
      report = isle.nextGeneration()
    }
  }
}


