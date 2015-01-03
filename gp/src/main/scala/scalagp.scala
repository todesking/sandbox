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

  def nextGeneration(): SelectionReport = {
    val (next, report) = selection.execute(individuals, population)
    this.individuals = next
    this._generation += 1
    report
  }
}

class SelectionReport

