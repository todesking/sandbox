package com.todesking.scalagp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.reflect.ClassTag

import Ext._

class Optimizer[C] {
  trait Matcher[A, D, N <: Computable[A, C]] {
    def apply(data: D): N
    def unapply(tree: Tree[_, C]): Option[N]
  }

  def newOptimizerNode[A: ClassTag, D, N <: Computable[A, C]](name: String)(create: D => N)(tryMatch: PartialFunction[Computable[A, C], N]) =
    new Matcher[A, D, N] {
      override def apply(data: D): N = create(data)
      override def unapply(tree: Tree[_, C]): Option[N] =
        if(tree.klass == implicitly[ClassTag[A]]) tryMatch.lift.apply(tree.ensureKlass(implicitly[ClassTag[A]]))
        else None
    }

  private[this] val optimizeRules = new ArrayBuffer[OptimizeRule[C]]

  def registerOptimizer(name: String)(ruleDef: PartialFunction[Tree[_, C], Computable[_, C]]): OptimizeRule[C] = {
    val rule = new OptimizeRule[C](name, ruleDef.lift)
    optimizeRules += rule
    rule
  }

  def disableOptimizer(rule: OptimizeRule[C]): Unit = {
    optimizeRules -= rule
  }

  def optimizerEnabled(rule: OptimizeRule[C]): Boolean =
    optimizeRules.contains(rule)

  def optimize[A](tree: Tree[A, C]): Tree[A, C] =
    tree match {
      case t: Leaf[A, C] =>
        optimize1(t)
      case t: Branch[A, C] =>
        val children = t.children.map(optimize(_))
        if(t.children.zip(children).forall { case(c, oc) => c eq oc })
          optimize1(t)
        else
          optimize1(t.definition.create(children))
      case t =>
        t
    }

  def optimize1[A](tree: Tree[A, C]): Tree[A, C] =
    optimizeRules.foldLeft[Tree[A, C]](tree) { (tree, rule) =>
      rule(tree) map { optimized =>
        tree.makeOptimized(optimized)
      } getOrElse tree
    }

}
