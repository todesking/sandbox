package com.todesking.scalagp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.reflect.ClassTag

import Ext._

case class Individual[A, C](tree: Tree[A, C]) {
  lazy val optimized: Tree[A, C] =
    tree.optimized
  def apply(ctx: C): A =
    optimized(ctx)
}

class Isle[A: ClassTag, C](
  val repository: Repository[C],
  val population: Int,
  val initialize: Initialize[A, C],
  val selection: Selection[A, C],
  val beforeSelection: Isle[A, C] => Unit = { _: Isle[A, C] => () }
) {
  def generation: Int =
    _generation

  private[this] var _generation: Int = 0

  var individuals: Seq[Individual[A, C]] =
    initialize.newIndividuals(repository, population)

  def nextGeneration(): SelectionReport[A, C] = {
    val start = System.nanoTime()
    beforeSelection(this)
    this.individuals = selection.execute(individuals, population)
    this._generation += 1
    SelectionReport(generation, individuals, System.nanoTime() - start)
  }
}

case class SelectionReport[A, C](generation: Int, individuals: Seq[Individual[A, C]], executionNanos: Long) {
  def executionMillis: Long = executionNanos / 1000 / 1000
  // returns (majolity, population)
  lazy val majolity: (Individual[A, C], Int) =
    individuals.groupBy(_.tree).toSeq.sortBy(_._2.size).map { x => (x._2.head, x._2.size) }.last
  lazy val uniqueIndividuals: Int = individuals.toSet.size
  def percentiles[O: Ordering](percents:Double*)(f: Individual[A, C] => O): Seq[(O, Individual[A, C])] = {
    val sorted = individuals.sortBy(f)
    percents.map { p =>
      val i = sorted((p * sorted.size / 100).toInt)
      f(i) -> i
    }
  }
}

class OptimizeRule[Ctx](name: String, rule: Tree[_, Ctx] => Option[Tree[_, Ctx] => OptimizedTree[_, Ctx, _]]) {
  def apply[A](tree: Tree[A, Ctx]): Option[Tree[_, Ctx] => OptimizedTree[_, Ctx, _]] =
    rule(tree)
}
