package com.todesking.scalagp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.reflect.ClassTag

import Ext._

trait Initialize[A, C] {
  def newIndividuals(repository: Repository[C], population: Int): Seq[Individual[A, C]]
}

object Initialize {
  def random[A: ClassTag, C](depth: Int)(implicit random: Random): Initialize[A, C] =
    new Initialize[A, C] {
      override def newIndividuals(repository: Repository[C], population: Int) =
        (0 to population) map { _ => Individual(repository.randomTree[A](depth)) }
    }
}

trait Selection[A, C] {
  def execute(individuals: Seq[Individual[A, C]], population: Int): Seq[Individual[A, C]]
}

object Selection {
  def default[A, C](tournament: Tournament[A, C])(implicit random: scala.util.Random): Selection[A, C] =
    new Selection[A, C] {
      override def execute(individuals: Seq[Individual[A, C]], population: Int): Seq[Individual[A, C]] = {
        val operations = Operations.default[A, C](tournament)
        val next = new ArrayBuffer[Individual[A, C]]
        while(next.size <= population) {
          next ++= operations.apply(individuals)
        }
        next
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
        group.zip(group.map(scoring(_))).maxBy(_._2)._1
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
        Seq(Individual(p1.replace(p2.value)), Individual(p2.replace(p1.value)))
      } getOrElse Seq.empty
    }
  }
  val mutation = new Operation {
    override def apply[A, C](individuals: Seq[Individual[A, C]], tournament: Tournament[A, C])(implicit random: Random) = {
      val target = tournament.fittest(individuals).tree
      val path = target.randomPath()
      path.value match {
        case _ if random.nextDouble < 0.1 =>
          Seq(Individual(path.replace(target.definition.repository.randomTree(10)(path.value.definition.klass, random))))
        case t: ConstLeaf[path.Value, path.Context] if random.nextDouble < 0.5 =>
          Seq(Individual(path.replace(mutateLeafValue(t))))
        case t: Leaf[path.Value, path.Context] =>
          Seq(Individual(path.replace(mutateLeafType(t))))
        case t: Branch[path.Value, path.Context, _] =>
          Seq(Individual(path.replace(mutateBranchType(t))))
      }
    }
    def mutateLeafValue[A, C](leaf: ConstLeaf[A, C]): ConstLeaf[A, C] =
      leaf.definition.create()
    def mutateLeafType[A, C](leaf: Leaf[A, C])(implicit random: Random): Leaf[A, C] = {
      leaf.definition.compatibleShapeDefinitions.sample().get.create()
    }
    def mutateBranchType[A, C, P](branch: Branch[A, C, P])(implicit random: Random): Branch[A, C, P] =
      branch.definition.compatibleShapeDefinitions.sample().get.create(branch.children)
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
            copy -> 1
          )
        definitions.weightedSampleBy(_._2).get._1.apply(individuals, tournament)
      }
    }
}

