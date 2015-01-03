import com.todesking.scalagp

object GP {
  val repository = new scalagp.Repository[Int]

  val random = new scala.util.Random

  object Tree {
    import repository._

    val const = registerConstLeaf[Int]("const",
      generateValue = () => random.nextInt(100),
      mutateValue = n => n + random.nextInt(3) - 1
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
      sampleRange.map { x => Math.pow((f(x) - indiv(x)).abs.min(1000), 2).toInt * -1 }.sum

    println(GP.repository.allDefinitions)

    val isle = new scalagp.Isle[Int, Int](
      repository = GP.repository,
      population = 1000,
      initialize = Initialize.random(10),
      selection = Selection.default(
        Tournament.maximizeScore(100) { individual => score(individual) }
      )
    )
    var generationStatus = isle.nextGeneration()
    while(isle.generation < 1000) {
      println(s"Generation ${isle.generation}")
      val sampled = isle.individuals.head
      println(s"sample: score=${score(sampled)}")
      println(sampled.tree.toString)
      generationStatus = isle.nextGeneration()
    }
  }
}


