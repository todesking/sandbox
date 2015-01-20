package com.todesking.scalagp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.reflect.ClassTag

import Ext._

class Repository[C] {
  val optimizer = new Optimizer[C]

  private[this] var _definitions =
    new ArrayBuffer[Definition[_, C]]

  private[this] def register[A <: Definition[_, C]](definition: A): A  = {
    if(definitionByName(definition.name).nonEmpty)
      throw new IllegalArgumentException(s""""${definition.name}" is already defined""")
    _definitions += definition
    definition
  }

  def disable(definition: Definition[_, C]): Unit =
    _definitions -= definition

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

  def definitionByName(name: String): Option[Definition[_, C]] =
    allDefinitions.filter(_.name == name).headOption

  def parse[A: ClassTag](s: String): Tree[A, C] =
    new Parser[A, C](this).parse(implicitly[ClassTag[A]], s)

  def uniformDistribution(): Distribution[C] =
    Distribution.uniform(this)

  def randomDistribution(weightRange: Range)(implicit random: Random): Distribution[C] = {
    val start = weightRange.start
    val end = weightRange.inclusive.end
    Distribution.weighted(
      allDefinitions.map { d =>
        d -> (start + random.nextInt(end - start))
      }
    )
  }

  private[this] def classNotRegistered[A: ClassTag] =
    throw new IllegalStateException(s"Tree definition for ${implicitly[ClassTag[A]].runtimeClass.getName} is not registered")
  override def equals(other: Any) = other match {
    case r: Repository[C] => this eq r
    case _ => false
  }
  override def hashCode = super.hashCode
}
