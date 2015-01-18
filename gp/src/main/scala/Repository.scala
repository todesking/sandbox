package com.todesking.scalagp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.reflect.ClassTag

import Ext._

class Repository[C] {
  private[this] var _definitions =
    new ArrayBuffer[Definition[_, C]]

  private[this] def register[A <: Definition[_, C]](definition: A): A  = {
    if(definitionByName(definition.name).nonEmpty)
      throw new IllegalArgumentException(s""""${definition.name}" is already defined""")
    _definitions += definition
    definition
  }

  def registerConstLeaf[A: ClassTag]
      (name: String, generateValue: () => A)
      : ConstLeafDefinition[A, C] =
    register(new ConstLeafDefinition[A, C](name, this, generateValue))

  def registerFunctionLeaf[A: ClassTag](name: String)(f: C => A): FunctionLeafDefinition[A, C] =
    register(new FunctionLeafDefinition[A, C](name, this, f))

  def registerBranch2[A: ClassTag, B1: ClassTag, B2: ClassTag]
      (name: String)
      (f: (C, Tree[B1, C], Tree[B2, C]) => A)
      : Branch2Definition[A, C, B1, B2] =
    register(new Branch2Definition[A, C, B1, B2](name, this, f))

  def registerBranch3[A: ClassTag, B1: ClassTag, B2: ClassTag, B3: ClassTag]
      (name: String)
      (f: (C, Tree[B1, C], Tree[B2, C], Tree[B3, C]) => A)
      : Branch3Definition[A, C, B1, B2, B3] =
    register(new Branch3Definition[A, C, B1, B2, B3](name, this, f))

  def registerOptimizerNode[A: ClassTag, D](name: String)(f: (C, D) => A): OptimizeDefinition[A, C, D] =
    new OptimizeDefinition[A, C, D](name, f, this)

  private[this] val optimizeRules = new ArrayBuffer[OptimizeRule[C]]

  def registerOptimizer[A: ClassTag, D](name: String)(ruleDef: PartialFunction[Tree[_, C], Tree[A, C] => OptimizedTree[A, C, D]]): OptimizeRule[C] = {
    val rule = new OptimizeRule[C](name, ruleDef.lift.asInstanceOf[Tree[_, C] => Option[Tree[_, C] => OptimizedTree[_, C, _]]])
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
    optimizeRules.foldLeft[Tree[A, C]](tree) { (tree, rule) => rule(tree) map(_(unoptimize(tree)).asInstanceOf[Tree[A, C]]) getOrElse tree }

  def unoptimize[A](tree: Tree[A, C]): Tree[A, C] =
    tree match {
      case t: Leaf[A, C] =>
        t
      case t: Branch[A, C] =>
        val children = t.children.map(unoptimize(_))
        if(t.children.zip(children).forall { case(c, oc) => c eq oc })
          t
        else
          t.definition.create(children)
      case t: OptimizedTree[A, C, _] =>
        t.wrapped
    }

  def allDefinitions: Traversable[Definition[_, C]] =
    _definitions

  def definitions[A: ClassTag]: Traversable[Definition[A, C]] =
    _definitions.filter { d =>
      implicitly[ClassTag[A]].runtimeClass.isAssignableFrom(d.klass.runtimeClass)
    }.asInstanceOf[Traversable[Definition[A, C]]]

  def leafDefinitions[A: ClassTag]: Traversable[LeafDefinition[A, C]] =
    _definitions
      .filter(_.isInstanceOf[LeafDefinition[_, _]])
      .asInstanceOf[Traversable[LeafDefinition[A, C]]]

  def randomTree[A: ClassTag](depth: Int)(implicit random: Random): Tree[A, C] =
    if(depth == 0) {
      leafDefinitions[A].toSeq.sample().get.create()
    } else {
      definitions[A].toSeq.sample().getOrElse(classNotRegistered[A]).randomTree(depth - 1)
    }

  def definitionByName(name: String): Option[Definition[_, C]] =
    allDefinitions.filter(_.name == name).headOption

  def parse[A: ClassTag](s: String): Tree[A, C] =
    new Parser[A, C](this).parse(implicitly[ClassTag[A]], s)

  def uniformDistribution(): Distribution[C] =
    Distribution.uniform(this)

  private[this] def classNotRegistered[A: ClassTag] =
    throw new IllegalStateException(s"Tree definition for ${implicitly[ClassTag[A]].runtimeClass.getName} is not registered")
  override def equals(other: Any) = other match {
    case r: Repository[C] => this eq r
    case _ => false
  }
  override def hashCode = super.hashCode
}
