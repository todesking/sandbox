package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{Method => JMethod}

import com.todesking.scalapp.syntax._

object Main {
  def main(args: Array[String]): Unit = {
  }
}

object Opt {
  def optimize[A <: AnyRef : ClassTag](orig: A): A = {
    val instance = Instance.Native[A](orig)
    instance.instance()
  }
}

class UnsupportedOpcodeException(byte: Int)
  extends RuntimeException(f"Unsupported opcode: 0x$byte%02X")

class AnalyzeException(msg: String) extends RuntimeException(msg)

object Util {
  def tsort[A, B](in: Seq[A])(labelOf: A => B)(depsOf: A => Set[B]): Seq[A] =
    tsort0(in.map { i => (i, labelOf(i), depsOf(i)) }, Set.empty, Seq.empty)

  private[this] def tsort0[A, B](in: Seq[(A, B, Set[B])], deps: Set[B], sorted: Seq[A]): Seq[A] =
    if(in.isEmpty) {
      sorted
    } else {
      val (nodep, dep) = in.partition { case (a, b, bs) => bs.forall(deps.contains) }
      tsort0(dep, deps ++ nodep.map(_._2), sorted ++ nodep.map(_._1))
    }
}

class LocalAllocator(preservedLocals: Seq[DataLabel])(aliasOf: DataLabel => Set[DataLabel]) {
  private[this] val allocated = mutable.HashMap.empty[DataLabel, Int]
  private[this] var nextLocal = preservedLocals.size

  preservedLocals.zipWithIndex foreach { case (l, i) =>
    aliasOf(l).foreach { a => allocated(a) = i }
  }

  def size: Int = nextLocal
  def apply(l: Some[DataLabel]): Int =
    apply(l.get)
  def apply(l: DataLabel): Int =
    allocated.get(l) getOrElse {
      val local = nextLocal
      aliasOf(l).foreach { a => allocated(a) = local }
      // TODO: support 2word
      nextLocal += 1
      local
    }
}


case class Frame(locals: Seq[DataLabel.Out], stack: List[DataLabel.Out], effect: Effect) {
  def local(n: Int): DataLabel.Out =
    locals(n)

  def dropStack(n: Int): Frame =
    if(stack.length < n) throw new IllegalArgumentException(s"Stack size is ${stack.size}, ${n} required.")
    else Frame(locals, stack.drop(n), effect)

  def pushStack(l: DataLabel.Out): Frame =
    Frame(locals, l +: stack, effect)

  def takeStack(n: Int): List[DataLabel.Out] =
    if(stack.length < n) throw new IllegalArgumentException(s"Stack size is ${stack.size}, ${n} required.")
    else stack.take(n)
}
object Frame {
  val empty = Frame(Seq.empty, List.empty, Effect.fresh)
}

case class Data(typeRef: TypeRef, value: Option[Any])
object Data {
  val Undefined = Data(TypeRef.Undefined, None)
}

case class FrameUpdate(
  newFrame: Frame,
  dataIn: Seq[(DataLabel.Out, DataLabel.In)] = Seq.empty
)

final class InstructionLabel private() extends AbstractLabel
object InstructionLabel {
  def fresh(): InstructionLabel = new InstructionLabel
}

final class JumpTarget private extends AbstractLabel
object JumpTarget extends AbstractLabel.AssignerProvider[JumpTarget] {
  def fresh(): JumpTarget = new JumpTarget
}

sealed abstract class DataLabel private(val name: String) extends AbstractLabel
object DataLabel extends AbstractLabel.NamerProvider[DataLabel] {
  final class In (name: String) extends DataLabel(name) {
    override def toString = s"DataLabel.In(${name})@${System.identityHashCode(this)}"
  }
  final class Out(name: String) extends DataLabel(name) {
    override def toString = s"DataLabel.Out(${name})@${System.identityHashCode(this)}"
  }

  def in(name: String) = new In(name)
  def out(name: String) = new Out(name)
}


final class Effect private extends AbstractLabel
object Effect {
  def fresh() = new Effect
}
