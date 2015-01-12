package com.todesking.scalagp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.reflect.ClassTag

trait TreePath[A, C, V] {
  import com.todesking.scalagp
  type Root = A
  type Context = C
  type Value = V

  def root: Tree[Root, Context]
  def value: Tree[Value, Context]
  def klass: ClassTag[Value] = value.definition.klass
  def replace(tree: Tree[Value, Context]): Tree[Root, Context]
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
        case _: Leaf[_, _] | _:OptimizedTree[_, _, _]   =>
          throw new AssertionError()
      }
    }
  }
}

