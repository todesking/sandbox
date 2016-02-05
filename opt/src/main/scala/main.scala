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

case class Dataflow(
  iNodes: Seq[Dataflow.INode],
  dataValues: Map[DataLabel, Data]
) {
}
object Dataflow {
  private[this] def newMultiMap[A, B] = new mutable.HashMap[A, mutable.Set[B]] with mutable.MultiMap[A, B]

  class Merger[P, L <: AbstractLabel](fresh: => L) {
    private[this] val merges = newMultiMap[(P, L), L]

    def merge(pos: P, l1: L, l2: L): L =
      if(l1 == l2) {
        l1
      } else if(merges.contains(pos -> l1)) {
        if(merges.contains(pos -> l2)) throw new AssertionError
        merges.addBinding(pos -> l1, l2)
        l2
      } else if(merges.contains(pos -> l2)) {
        merges.addBinding(pos -> l2, l1)
        l1
      } else {
        val m = fresh
        merges.addBinding(pos -> m, l1)
        merges.addBinding(pos -> m, l2)
        m
      }
  }

  def build(body: MethodBody): Dataflow = {
    val binding = mutable.HashMap.empty[DataLabel.In, DataLabel.Out]

    val thisLabel = if(body.isStatic) None else Some(DataLabel.out("this"))
    val argLabels = body.descriptor.args.zipWithIndex.map { case (_, i) => DataLabel.out(s"arg_${i}") }
    val initialEffect = Effect.fresh()

    val initialFrame =
      Frame((thisLabel.toSeq ++ argLabels).zipWithIndex.map(_.swap).toMap, List.empty, initialEffect)

    val tasks = mutable.Set.empty[(Bytecode.Label, Frame)]
    val preFrames = mutable.HashMap.empty[Bytecode.Label, Frame]

    tasks += (body.bytecode.head.label -> initialFrame)

    val effectDependencies = mutable.HashMap.empty[Effect, Effect]

    val dataMerges = new Merger[Bytecode.Label, DataLabel.Out](DataLabel.out("merged"))

    val effectMerges = new Merger[Bytecode.Label, Effect](Effect.fresh())

    def merge(pos: Bytecode.Label, f1: Frame, f2: Frame): Frame = {
      Frame(
        (f1.locals.keySet ++ f2.locals.keySet)
          .filter { k => f1.locals.contains(k) && f2.locals.contains(k) }
          .map { k => (k -> dataMerges.merge(pos, f1.locals(k), f2.locals(k))) }.toMap,
        f1.stack.zip(f2.stack).map { case(a, b) => dataMerges.merge(pos, a, b) },
        effectMerges.merge(pos, f1.effect, f2.effect)
      )
    }

    val liveBcs = mutable.HashMap.empty[Bytecode.Label, Bytecode]

    while(tasks.nonEmpty) {
      val (pos, frame) = tasks.head
      tasks.remove(pos -> frame)
      val merged = preFrames.get(pos).map(merge(pos, _, frame)) getOrElse frame
      if(preFrames.get(pos).map(_ != merged) getOrElse true) {
        preFrames(pos) = merged
        val bseq = body.bytecode.dropWhile(_.label != pos)
        liveBcs(bseq.head.label) = bseq.head
        val u = bseq.head.nextFrame(merged)
        binding ++= u.binding
        bseq.head match {
          case j: Bytecode.Jump =>
            tasks += (body.jumpTargets(j.target) -> u.newFrame)
          case b:  Bytecode.Branch =>
            tasks += (body.jumpTargets(b.target) -> u.newFrame)
            tasks += (bseq(1).label -> u.newFrame)
          case r: Bytecode.XReturn =>
          case _: Bytecode.Procedure | _: Bytecode.Shuffle =>
            tasks += (bseq(1).label -> u.newFrame)
        }
      }
    }
    // now we have complete binding and data merges

    val dataValues = mutable.HashMap.empty[DataLabel, Data]
    dataValues ++= thisLabel.map { th => (th -> Data(TypeRef.This, None)) }
    dataValues ++= argLabels.zip(body.descriptor.args).map { case (l, t) =>
      (l -> Data(t, None))
    }
    val out2bc = body.bytecode.flatMap { bc =>
      bc.output.map { o => (o -> bc.label) }
    }.toMap
    Util.tsort(liveBcs.values.toSeq)(_.label) { bc =>
      bc.inputs.map(binding).flatMap(out2bc.get).toSet
    } foreach { bc =>
      dataValues ++= bc.inputs.map { i => (i -> dataValues(binding(i))) }
      dataValues ++= bc.outValue(dataValues)
    }

    val inodes = liveBcs.values collect {
      case ret :Bytecode.XReturn =>
        INode.Return()(ret.in, ret.effect)
      case iv @ Bytecode.invokevirtual(className, methodRef) =>
        INode.InvokeVirtual(
          className,
          methodRef,
          dataValues(iv.receiver),
          iv.ret.map(dataValues),
          iv.args.map(dataValues): _*
        )(
          iv.receiver,
          iv.ret,
          iv.args,
          iv.effect
        )
    }

    Dataflow(inodes.toSeq, dataValues.toMap)
  }
  sealed abstract class INode {
    final val label: INode.Label = INode.Label.fresh()
    def inputs: Seq[DataLabel.In]
    def output: Option[DataLabel.Out]
    def effect: Option[Effect]
  }
  object INode {
    final class Label private() extends AbstractLabel
    object Label {
      def fresh(): Label = new Label
    }

    case class Return()(ret: DataLabel.In, eff: Effect) extends INode {
      override val inputs = Seq(ret)
      override val output = None
      override val effect = Some(eff)
    }
    case class InvokeVirtual(
      className: ClassName,
      method: LocalMethodRef,
      receiver: Data,
      ret: Option[Data],
      args: Data*
    )(
      receiverLabel: DataLabel.In,
      retLabel: Option[DataLabel.Out],
      argLabels: Seq[DataLabel.In],
      eff: Effect
    )extends INode {
      require(!(method.isVoid ^ retLabel.isEmpty))
      require(method.args.size == argLabels.size)

      override def inputs = argLabels
      override def output = retLabel
      override def effect = Some(eff)
    }
    case class WhiteHole(out: DataLabel.Out) extends INode {
      override def inputs = Seq.empty
      override def output = Some(out)
      override val effect = Some(Effect.fresh())
    }
    case class BlackHole(in: DataLabel.In) extends INode {
      override def inputs = Seq(in)
      override def output = None
      override val effect = Some(Effect.fresh())
    }
  }
}
