package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import java.lang.reflect.{ Method => JMethod }

import com.todesking.scalapp.syntax._

object Main {
  def main(args: Array[String]): Unit = {
  }
}

object Opt {
  def optimize[A <: AnyRef: ClassTag](orig: A): A = {
    val instance = Instance.Native[A](orig)
    instance.instance()
  }
}

object Util {
  def tsort[A, B](in: Seq[A])(labelOf: A => B)(depsOf: A => Set[B]): Seq[A] =
    tsort0(in.map { i => (i, labelOf(i), depsOf(i)) }, Set.empty, Seq.empty)

  private[this] def tsort0[A, B](in: Seq[(A, B, Set[B])], deps: Set[B], sorted: Seq[A]): Seq[A] =
    if (in.isEmpty) {
      sorted
    } else {
      val (nodep, dep) = in.partition { case (a, b, bs) => bs.forall(deps.contains) }
      tsort0(dep, deps ++ nodep.map(_._2), sorted ++ nodep.map(_._1))
    }
}

class LocalAllocator(preservedLocals: Seq[DataLabel])(aliasOf: DataLabel => Set[DataLabel]) {
  private[this] val allocated = mutable.HashMap.empty[DataLabel, Int]
  private[this] var nextLocal = preservedLocals.size

  preservedLocals.zipWithIndex foreach {
    case (l, i) =>
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

case class Frame(locals: Map[Int, DataLabel.Out], stack: List[DataLabel.Out], effect: Effect) {
  def local(n: Int): DataLabel.Out =
    locals(n)

  def stackTop: DataLabel.Out = stack.head

  def update: FrameUpdate = FrameUpdate(this, Map.empty)
}
object Frame {
  val empty = Frame(Map.empty, List.empty, Effect.fresh())
}

case class Data(typeRef: TypeRef, value: Option[Any])
object Data {
  val Undefined = Data(TypeRef.Undefined, None)
}

case class FrameUpdate(
  newFrame: Frame,
  binding: Map[DataLabel.In, DataLabel.Out]
) {
  def pop1(l: DataLabel.In): FrameUpdate =
    FrameUpdate(
      newFrame.copy(stack = newFrame.stack.tail),
      binding + (l -> newFrame.stack.head)
    )

  def pop2(l: DataLabel.In): FrameUpdate =
    FrameUpdate(
      newFrame.copy(stack = newFrame.stack.drop(2)),
      binding + (l -> newFrame.stack(1))
    )

  def setLocal(n: Int, data: DataLabel.Out): FrameUpdate =
    FrameUpdate(newFrame.copy(locals = newFrame.locals.updated(n, data)), binding)

  def load1(n: Int): FrameUpdate = push1(newFrame.local(n))

  def store1(n: Int): FrameUpdate = setLocal(n, newFrame.stackTop)

  def push1(d: DataLabel.Out): FrameUpdate =
    FrameUpdate(newFrame.copy(stack = d +: newFrame.stack), binding)

  def push2(d: DataLabel.Out): FrameUpdate =
    FrameUpdate(newFrame.copy(stack = DataLabel.out(s"second word of ${d.name}") :: d :: newFrame.stack), binding)

  def push(t: TypeRef, d: DataLabel.Out): FrameUpdate =
    if(t.isDoubleWord) push2(d) else push1(d)

  // TODO 2word
  def ret(retval: DataLabel.In): FrameUpdate =
    FrameUpdate(
      Frame.empty.copy(effect = newFrame.effect),
      binding + (retval -> newFrame.stack.head)
    )
}
object FrameUpdate {
}

final class InstructionLabel private () extends AbstractLabel
object InstructionLabel {
  def fresh(): InstructionLabel = new InstructionLabel
}

final class JumpTarget private extends AbstractLabel
object JumpTarget extends AbstractLabel.AssignerProvider[JumpTarget] {
  def fresh(): JumpTarget = new JumpTarget
}

sealed abstract class DataLabel private (val name: String) extends AbstractLabel
object DataLabel extends AbstractLabel.NamerProvider[DataLabel] {
  final class In(name: String) extends DataLabel(name) {
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

trait Transformer[A <: AnyRef, B <: AnyRef] {
  def apply(orig: Instance[A]): Try[Instance[B]]
}
object Transformer {
  def changeBaseClass[A <: AnyRef](baseClass: Class[A]): Transformer[A, A] = new Transformer[A, A] {
    override def apply(orig: Instance[A]): Try[Instance[A]] = {
      try {
        // TODO: handle protected/package method
        val newInstance = Instance.New(baseClass)

        val required = newInstance.methods.flatMap { m => requiredMethods(orig, newInstance, m) }

        Success(Instance.Rewritten(
          newInstance,
          required.map { m =>
            val body = orig.methodBody(m) getOrElse { throw new TransformError(s"Can't acquire method body for ${m}") }
            m -> body
          }.toMap
        ))
      } catch {
        case e: TransformError => Failure(e)
      }
    }

    private[this] def requiredMethods(
      orig: Instance[_ <: AnyRef],
      newInstance: Instance[_ <: AnyRef],
      m: LocalMethodRef,
      required: Set[LocalMethodRef] = Set.empty
    ): Set[LocalMethodRef] = {
      if(required.contains(m)) required
      else if(newInstance.sameMethodDefined(m, orig)) required
      else if(orig.isNativeMethod(m)) throw new TransformError(s"Method ${m.str} in ${orig.baseClass} is native")
      else if(orig.isAbstractMethod(m)) required
      else orig.methodBody(m).map { body =>
        import Dataflow.INode._
        body.dataflow.iNodes.collect {
          case InvokeVirtual(className, method, Data(TypeRef.This, _), retOption, args @ _*) =>
            method
          }.distinct.foldLeft(required) { (r, m) => (r + m) ++ requiredMethods(orig, newInstance, m, (r + m)) } + m
        // TODO: check other instance's protected/package private
        // TODO: check same-instance method call(virtual/special)
      }.getOrElse { throw new AssertionError() }
    }
  }
}

