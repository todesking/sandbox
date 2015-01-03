package com.todesking.scalagp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import Ext._

case class Individual[A, C](tree: Tree[A, C]) {
  def apply(ctx: C): A =
    tree(ctx)
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

  def nextGeneration(): SelectionReport[A, C] = {
    this.individuals = selection.execute(individuals, population)
    this._generation += 1
    SelectionReport(generation, individuals)
  }
}

case class SelectionReport[A, C](generation: Int, individuals: Seq[Individual[A, C]]) {
  lazy val uniqueIndividuals: Int = individuals.toSet.size
  def percentiles[O: Ordering](percents:Double*)(f: Individual[A, C] => O): Seq[(O, Individual[A, C])] = {
    val sorted = individuals.sortBy(f)
    percents.map { p =>
      val i = sorted((p * sorted.size / 100).toInt)
      f(i) -> i
    }
  }
}

