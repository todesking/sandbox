package com.todesking.gp

// Building Block
package bb {
  import scala.collection.mutable.ArrayBuffer
  import scala.util.Random

  object Implicits {
    implicit val intClass = classOf[Int]
  }

  object Ext {
    implicit class SeqExt[A](self: Seq[A]) {
      def weightedSampleBy(f: A => Int)(implicit random: Random): Option[A] = {
        val withWeight = self.zip(self.map(f(_)))
        val sum = withWeight.map(_._2).sum
        val th = random.nextInt(sum)
        var weight = 0
        withWeight.foreach { case(elm, w) =>
          weight += w
          if(th < weight) return Some(elm)
        }
        None
      }
      def sample()(implicit random: Random): Option[A] =
        if(self.isEmpty) None
        else Some(self(random.nextInt(self.size)))
      def sample(size: Int)(implicit random: Random): Traversable[A] = {
        require(size <= self.size)
        self.zip((0 to self.size).map { _ => random.nextDouble }).sortBy(_._2).slice(0 ,size).map(_._1)
      }
    }
  }
  import Ext._

  case class Individual[A, C](tree: Tree[A, C]) {
    def apply(ctx: C): A =
      tree(ctx)
  }

  sealed abstract class Tree[A, C] {
    def definition: Definition[A, C, Tree[A, C]]
    def apply(ctx: C): A
    val height: Int
    val size: Int
    def allPaths: Traversable[TreePath[A, C, _]] =
      allPaths(TreePath.Root(this))
    def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, _]]
    def randomPath()(implicit random: Random): TreePath[A, C, _] =
      allPaths.toSeq.sample().get
    def randomPath[X](klass: Class[X])(implicit random: Random): Option[TreePath[A, C, X]] = {
      allPaths.filter { p =>
        klass.isAssignableFrom(p.value.definition.klass)
      }.toSeq.sample().map(_.asInstanceOf[TreePath[A, C, X]])
    }
  }
  trait TreePath[A, C, V] {
    type RootType = A
    type ContextType = C
    type ValueType = V

    def root: Tree[A, C]
    def value: Tree[V, C]
    def klass: Class[_ <: V] = value.definition.klass
    def replace(tree: Tree[V, C]): Tree[A, C]
    def unsafeReplace(tree: Tree[_, C]): Tree[A, C] =
      replace(tree.asInstanceOf[Tree[V, C]])
  }
  object TreePath {
    case class Root[A, C](override val root: Tree[A, C]) extends TreePath[A, C, A] {
      override def value: Tree[A, C] = root
      override def replace(tree: Tree[A, C]): Tree[A, C] = tree
    }
    case class Descent[A, C, V, PV](parent: TreePath[A, C, PV], index: Int, override val value: Tree[V, C]) extends TreePath[A, C, V] {
      override def root = parent.root
      override def replace(tree: Tree[V, C]): Tree[A, C] = {
        parent.value match {
          case b: Branch[PV, C, _] =>
            parent.replace(b.replaceChild(index, tree))
          case l: Leaf[_, C] =>
            throw new AssertionError()
        }
      }
    }
  }
  abstract class Leaf[A, C] extends Tree[A, C] {
    override def definition: LeafDefinition[A, C, Leaf[A, C]]
    override val height = 0
    override val size = 1
    override def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, A]] =
      Seq(base)
  }
  object Leaf {
    def unapply[A, C](l: Leaf[A, C]): Option[Class[_ <: A]] =
      Some(l.definition.klass)
  }
  class ConstLeaf[A, C](val value: A, override val definition: LeafDefinition[A, C, ConstLeaf[A, C]]) extends Leaf[A, C] {
    override def apply(ctx: C): A = value
    override def toString =
      value.toString
  }
  object ConstLeaf {
    def unapply[A, C](cl: ConstLeaf[A, C]): Option[(Class[_ <: A], A)] =
      Some(cl.definition.klass -> cl.value)
  }
  class FunctionLeaf[A, C](val function: C => A, override val definition: LeafDefinition[A, C, FunctionLeaf[A, C]]) extends Leaf[A, C] {
    override def apply(ctx: C): A =
      function(ctx)
    override def toString =
      s"(${definition.name})"
  }
  sealed abstract class Branch[A, C, P](
    val function: ((C, P)) => A
  ) extends Tree[A, C] {
    def children: Seq[Tree[_, C]]
    def childrenParam: P
    override def definition: BranchDefinition[A, C, Branch[A, C, P]]
    override lazy val height = children.map(_.height).max
    override lazy val size = children.map(_.size).sum + 1
    override def apply(ctx: C): A =
      function((ctx, childrenParam))
    def replaceChild(index: Int, c: Tree[_, C]): Branch[A, C, P] =
      definition.create(children.updated(index, c))
    override def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, _]] =
      base +: children.zipWithIndex.flatMap { case (c: Tree[_, C], i) => c.allPaths(TreePath.Descent(base, i, c)) }
    override def toString =
      s"(${definition.name} ${children.map(_.toString).mkString(" ")})"
  }
  class Branch2[A, C, B1, B2](
    override val definition: BranchDefinition[A, C, Branch2[A, C, B1, B2]],
    val child1: Tree[B1, C],
    val child2: Tree[B2, C],
    function: ((C,(Tree[B1, C], Tree[B2, C]))) => A
  ) extends Branch[A, C, (Tree[B1, C], Tree[B2, C])](function) {
    override val children = Seq(child1, child2)
    override val childrenParam = (child1, child2)
  }
  object Branch {
    def unapply[A, C, P](b: Branch[A, C, P]): Option[Class[_ <: A]] =
      Some(b.definition.klass)
  }

  abstract class Definition[A, C, +T <: Tree[A, C]](val name: String, val klass: Class[_ <: A], repository: Repository[C]) {
    type InstanceType = T
    def arity: Int = childClasses.size
    def childClasses: Seq[Class[_]]
    def randomTree(repository: Repository[C], depth: Int)(implicit random: Random): T
    def compatibleShapeDefinitions(): Traversable[Definition[A, C, T]] =
      repository.definitions(klass).filter { d =>
        d.arity == arity && d.childClasses.zip(childClasses).forall { case (a, b) => a.isAssignableFrom(b) }
      }.asInstanceOf[Traversable[Definition[A, C, T]]]
  }
  abstract class BranchDefinition[A, C, +T <: Branch[A, C, _]](name: String, klass: Class[_ <: A], repository: Repository[C]) extends Definition[A, C, T](name, klass, repository) {
    def create(children: Seq[Tree[_, C]]): T
    override def toString() =
      s"Definition(${name}, ${klass.getName}, [${childClasses.map(_.getName).mkString(", ")}](${arity}))"
  }
  abstract class LeafDefinition[A, C, +T <: Leaf[A, C]](name: String, klass: Class[_ <: A], repository: Repository[C]) extends Definition[A, C, T](name, klass, repository) {
    override val childClasses = Seq.empty
    def create(): T
    override def randomTree(repository: Repository[C], depth: Int)(implicit random: Random): InstanceType =
      create()
    override def toString() =
      s"Definition(${name}, ${klass.getName})"
  }

  class Repository[Ctx] {
    implicit val Int = classOf[Int]
    private[this] var _definitions = new ArrayBuffer[Definition[_, Ctx, Tree[_, Ctx]]]
    private[this] def register[A <: Definition[_, Ctx, Tree[_, Ctx]]](definition: A): A  = {
      _definitions += definition
      definition
    }
    def registerConstLeaf[A: Class](name: String, generateValue: () => A, mutateValue: A => A): Definition[A, Ctx, ConstLeaf[A, Ctx]] = register(
      new LeafDefinition[A, Ctx, ConstLeaf[A, Ctx]](name, implicitly[Class[A]], this) {
        override def create(): ConstLeaf[A, Ctx] =
          new ConstLeaf(generateValue(), this)
      }
    )

    def registerLeaf[A: Class](name: String)(f: Ctx => A): Definition[A, Ctx, FunctionLeaf[A, Ctx]] = register(
      new LeafDefinition[A, Ctx, FunctionLeaf[A, Ctx]](name, implicitly[Class[A]], this) {
        override def create() =
          new FunctionLeaf(f, this)
      }
    )

    def registerBranch2[A: Class, B1, B2](name: String)(f: ((Ctx, (Tree[B1, Ctx], Tree[B2, Ctx]))) => A)(implicit c1: Class[B1], c2: Class[B2]): Definition[A, Ctx, Branch2[A, Ctx, B1, B2]] = register(
      new BranchDefinition[A, Ctx, Branch2[A, Ctx, B1, B2]](name, implicitly[Class[A]], this) {
        override val childClasses = Seq(c1, c2)
        override def create(children: Seq[Tree[_, Ctx]]): Branch2[A, Ctx, B1, B2] = {
          require(children.size == 2)
          require(children.zip(childClasses).forall { case(c, k) => k.isAssignableFrom(c.definition.klass) })
          new Branch2(
            this,
            children(0).asInstanceOf[Tree[B1, Ctx]],
            children(1).asInstanceOf[Tree[B2, Ctx]],
            f
          )
        }
        override def randomTree(repository: Repository[Ctx], depth: Int)(implicit random: Random): InstanceType = {
          create(Seq(
            repository.randomTree[B1](depth - 1),
            repository.randomTree[B2](depth - 1)
          ))
        }
      }
    )

    def allDefinitions: Traversable[Definition[_, Ctx, Tree[_, Ctx]]] =
      _definitions

    def definitions[A: Class]: Traversable[Definition[A, Ctx, Tree[A, Ctx]]] =
      _definitions.filter{d => implicitly[Class[A]].isAssignableFrom(d.klass)}.asInstanceOf[Traversable[Definition[A, Ctx, Tree[A, Ctx]]]]

    def leafDefinitions[A: Class]: Traversable[LeafDefinition[A, Ctx, _ <: Leaf[A, Ctx]]] =
      _definitions.filter(_.isInstanceOf[LeafDefinition[_, _, _]]).asInstanceOf[Traversable[LeafDefinition[A, Ctx, _ <: Leaf[A, Ctx]]]]

    def randomTree[A: Class](depth: Int)(implicit random: Random): Tree[A, Ctx] =
      if(depth == 0) {
        leafDefinitions[A].toSeq.sample().get.create()
      } else {
        definitions[A].toSeq.sample().getOrElse(throw classNotRegistered[A]).randomTree(this, depth)
      }

    private[this] def classNotRegistered[A](implicit klass: Class[A]) =
      invalid(s"Tree definition for ${implicitly[Class[A]].getName} is not registered")
    private[this] def invalid(msg: String) = new IllegalStateException(msg)
  }

  class Isle[A, C](
    val repository: Repository[C],
    val population: Int,
    val initialize: Initialize[A, C],
    val selection: Selection[A, C]
  )(implicit ev: Class[A]) {
    def generation: Int =
      _generation

    private[this] var _generation: Int = 0

    var individuals: Seq[Individual[A, C]] =
      initialize.newIndividuals(repository, population)

    def nextGeneration(): SelectionReport = {
      val (next, report) = selection.execute(individuals, population)
      this.individuals = next
      this._generation += 1
      report
    }
  }

  trait Initialize[A, C] {
    def newIndividuals(repository: Repository[C], population: Int): Seq[Individual[A, C]]
  }

  object Initialize {
    def random[A: Class, C](depth: Int)(implicit random: Random): Initialize[A, C] =
      new Initialize[A, C] {
        override def newIndividuals(repository: Repository[C], population: Int) =
          (0 to population) map { _ => Individual(repository.randomTree[A](depth)) }
      }
  }

  trait Selection[A, C] {
    def execute(individuals: Seq[Individual[A, C]], population: Int): (Seq[Individual[A, C]], SelectionReport)
  }

  object Selection {
    def default[A, C](tournament: Tournament[A, C])(implicit random: scala.util.Random): Selection[A, C] =
      new Selection[A, C] {
        override def execute(individuals: Seq[Individual[A, C]], population: Int): (Seq[Individual[A, C]], SelectionReport) = {
          val operations = Operations.default[A, C](tournament)
          val next = new ArrayBuffer[Individual[A, C]]
          while(next.size <= population) {
            next ++= operations.apply(individuals)
          }
          (next, new SelectionReport)
        }
      }
  }

  trait Tournament[A, C] {
    def fittest(individuals: Seq[Individual[A, C]]): Individual[A, C]
  }
  object Tournament {
    def maximizeScore[A, C, O](size: Int)(scoring: Individual[A, C] => O)(implicit random: Random, ordering: Ordering[O]) =
      new Tournament[A, C] {
        override def fittest(individuals: Seq[Individual[A, C]]): Individual[A, C] = {
          require(individuals.nonEmpty)
          val group = individuals.sample(size).toSeq
          group.maxBy(scoring(_))
        }
      }
  }

  trait Operations[A, C] {
    def apply(individuals: Seq[Individual[A, C]])(implicit random: Random): Traversable[Individual[A, C]]
  }
  trait Operation {
    def apply[A, C](individuals: Seq[Individual[A, C]], tournament: Tournament[A, C])(implicit random: Random): Traversable[Individual[A, C]]
  }
  object Operations {
    val crossover = new Operation {
      override def apply[A, C](individuals: Seq[Individual[A, C]], tournament: Tournament[A, C])(implicit random: Random) = {
        val p1: TreePath[A, C, _] = tournament.fittest(individuals).tree.randomPath()
        tournament.fittest(individuals).tree.randomPath(p1.klass) map { p2 =>
          Seq(Individual(p1.unsafeReplace(p2.value)), Individual(p2.unsafeReplace(p1.value)))
        } getOrElse Seq.empty
      }
    }
    val mutation = new Operation {
      override def apply[A, C](individuals: Seq[Individual[A, C]], tournament: Tournament[A, C])(implicit random: Random) = {
        val target = tournament.fittest(individuals).tree
        val path = target.randomPath()
        path.value match {
          case cl@ConstLeaf(klass, value) if true =>
            Seq(Individual(path.unsafeReplace(mutateLeafValue(cl))))
          case l@Leaf(klass) =>
            Seq(Individual(path.unsafeReplace(mutateLeafType(l))))
          case b@Branch(klass) =>
            Seq(Individual(path.unsafeReplace(mutateBranchType(b))))
        }
      }
      def mutateLeafValue[A, C](leaf: ConstLeaf[A, C]): ConstLeaf[A, C] =
        leaf.definition.create()
      def mutateLeafType[A, C](leaf: Leaf[A, C])(implicit  random: Random): Leaf[A, C] = {
        leaf.definition.compatibleShapeDefinitions.toSeq.sample().get.asInstanceOf[LeafDefinition[A, C, Leaf[A, C]]].create()
      }
      def mutateBranchType[A, C, P](branch: Branch[A, C, P])(implicit random: Random): Branch[A, C, P] =
        branch.definition.compatibleShapeDefinitions.toSeq.sample.get.asInstanceOf[BranchDefinition[A, C, Branch[A, C, P]]].create(branch.children)
    }
    val copy = new Operation {
      override def apply[A, C](individuals: Seq[Individual[A, C]], tournament: Tournament[A, C])(implicit random: Random) =
        Seq(tournament.fittest(individuals))
    }
    def default[A, C](tournament: Tournament[A, C])(implicit random: scala.util.Random): Operations[A, C] =
      new Operations[A, C] {
        override def apply(individuals: Seq[Individual[A, C]])(implicit random: Random): Traversable[Individual[A, C]] = {
          val definitions: Seq[(Operation, Int)] =
            Seq(
              crossover -> 90,
              mutation -> 9,
              copy -> 100
            )
          definitions.weightedSampleBy(_._2).get._1.apply(individuals, tournament)
        }
      }
  }

  class SelectionReport
}

object GP {
  val repository = new bb.Repository[Int]

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
    import bb.{Individual, Initialize, Selection, Tournament}
    import bb.Implicits._

    implicit val random = new scala.util.Random

    def f(x: Int) = x * x * x - 3 * x * x + 10 * x - 5

    val sampleRange = (-10 to 10)
    def score(indiv: Individual[Int, Int]): Int =
      sampleRange.map { x => Math.pow((f(x) - indiv(x)).abs.min(1000), 2).toInt * -1 }.sum

    println(GP.repository.allDefinitions)

    val isle = new bb.Isle[Int, Int](
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


