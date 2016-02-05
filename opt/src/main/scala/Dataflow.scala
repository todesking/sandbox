package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import java.lang.reflect.{ Method => JMethod }

import com.todesking.scalapp.syntax._

case class Dataflow(
  isStatic: Boolean,
  descriptor: MethodDescriptor,
  iNodes: Seq[Dataflow.INode],
  dataDependencies: Map[DataLabel.In, DataLabel.Out],
  effectDependencies: Map[Effect, Set[Effect]],
  dataValues: Map[DataLabel, Data]
) {
  import Dataflow.INode

  def data(l: DataLabel): Data = dataValues(l)

  def compile(): MethodBody = {
    println(toDot)
    ???
  }

  private[this] val out2inode = iNodes.flatMap { inode => inode.output.map { o => (o -> inode) } }.toMap

  def iNode(l: INode.Label): INode =
    iNodes.find(_.label == l).get

  def dependentINodes(inode: INode): Seq[INode] =
    inode.inputs.map(dataDependencies).flatMap(out2inode.get)

  def toDot: String = {
    def drawNode(id: String, label: String) = s"""${id}[label="${label}"]"""
    def drawEdge(from: String, to: String) = s"""${from} -> ${to}"""
    val dataName = DataLabel.namer("data_", "Data ")
    val inodeName = INode.Label.namer("inode_", "INode ")
    val effName = Effect.namer("effect_", "Effect ")
    s"""digraph {
graph[rankdir="BT"]
${
  iNodes.map { inode => drawNode(inodeName.id(inode.label), inode.pretty) }.mkString("\n")
}
${
  (dataValues.keys ++ iNodes.flatMap { i => i.inputs ++ i.output }).toSet.map { d: DataLabel =>
    drawNode(dataName.id(d), s"""${d.name}: ${dataValues.get(d).map(_.pretty) getOrElse "ERROR: Not Found"}""")
  }.mkString("\n")
}
${
  (iNodes.flatMap(_.effect) ++ effectDependencies.flatMap { case (k, vs) => vs + k }).distinct.map { eff =>
    drawNode(effName.id(eff), effName.name(eff))
  }.mkString("\n")
}
${
  dataDependencies.map { case (from, to) =>
    drawEdge(dataName.id(from), dataName.id(to))
  }.mkString("\n")
}
${
  iNodes.flatMap { inode =>
    inode.inputs.map { in =>
      drawEdge(inodeName.id(inode.label), dataName.id(in))
    } ++ inode.output.map { out =>
      drawEdge(dataName.id(out), inodeName.id(inode.label))
    }
  }.mkString("\n")
}
${
  effectDependencies.flatMap { case (from, tos) =>
    tos.map { to =>
      drawEdge(effName.id(from), effName.id(to))
    }
  }.mkString("\n")
}
${
  iNodes.flatMap { inode =>
    inode.effect.map {eff => drawEdge(effName.id(eff), inodeName.id(inode.label)) }
  }.mkString("\n")
}
}"""
    }
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
    val thisLabel = if(body.isStatic) None else Some(DataLabel.out("this"))
    val argLabels = body.descriptor.args.zipWithIndex.map { case (_, i) => DataLabel.out(s"arg_${i}") }
    val initialEffect = Effect.fresh()

    val (binding, liveBcs) = analyze(body, thisLabel, argLabels, initialEffect)

    val dataValues = mutable.HashMap.empty[DataLabel, Data]
    dataValues ++= thisLabel.map { th => (th -> Data(TypeRef.This, None)) }
    dataValues ++= argLabels.zip(body.descriptor.args).map { case (l, t) =>
      (l -> Data(t, None))
    }
    val out2bc = body.bytecode.flatMap { bc =>
      bc.output.map { o => (o -> bc.label) }
    }.toMap
    Util.tsort(liveBcs)(_.label) { bc =>
      bc.inputs.map(binding).flatMap(out2bc.get).toSet
    } foreach { bc =>
      dataValues ++= bc.inputs.map { i => (i -> dataValues(binding(i))) }
      dataValues ++= bc.outValue(dataValues)
    }

    val inodes = liveBcs collect {
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

    Dataflow(
      body.isStatic,
      body.descriptor,
      inodes.toSeq,
      binding.toMap,
      Map.empty,
      dataValues.toMap
    )
  }

  private[this] def analyze(
    body: MethodBody,
    thisLabel: Option[DataLabel.Out],
    argLabels: Seq[DataLabel.Out],
    initialEffect: Effect
  ): (Map[DataLabel.In, DataLabel.Out], Seq[Bytecode]) = {
    val binding = mutable.HashMap.empty[DataLabel.In, DataLabel.Out]

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

    (binding.toMap -> liveBcs.values.toSeq)
  }


  sealed abstract class INode {
    final val label: INode.Label = INode.Label.fresh()
    def inputs: Seq[DataLabel.In]
    def output: Option[DataLabel.Out]
    def effect: Option[Effect]
    def pretty: String = toString
  }
  object INode {
    final class Label private() extends AbstractLabel
    object Label extends AbstractLabel.NamerProvider[Label] {
      def fresh(): Label = new Label
    }

    case class Return()(ret: DataLabel.In, eff: Effect) extends INode {
      override val inputs = Seq(ret)
      override val output = None
      override val effect = Some(eff)
      override def pretty = "return"
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
      override def pretty = s"""invokevirtual ${className.binaryString}#${method.str}"""
    }
    case class WhiteHole(out: DataLabel.Out) extends INode {
      override def inputs = Seq.empty
      override def output = Some(out)
      override val effect = Some(Effect.fresh())
      override def pretty = "WhiteHole"
    }
    case class BlackHole(in: DataLabel.In) extends INode {
      override def inputs = Seq(in)
      override def output = None
      override val effect = Some(Effect.fresh())
      override def pretty = "BlackHole"
    }
  }
}
