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
  effectDependencies: Map[Dataflow.INode.Label, Effect],
  dataValues: Map[DataLabel, Data],
  jumpTargets: Map[JumpTarget, Dataflow.INode.Label],
  dataMerges: Map[DataLabel.Out, Set[DataLabel.Out]],
  effectMerges: Map[Effect, Set[Effect]]
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
    import Graphviz._
    val dataName = DataLabel.namer("data_", "Data ")
    val inodeName = INode.Label.namer("inode_", "INode ")
    val effName = Effect.namer("effect_", "Effect ")
    def jumpTargets(inode: INode): Seq[INode.Label] = (inode match {
      case i: INode.Goto => Seq(i.target)
      case i: INode.ICmp => Seq(i.thenTarget)
      case _ => Seq.empty
      }).map(this.jumpTargets)
    val bhs = iNodes.collect { case bh: INode.BlackHole => bh }
    val whs = iNodes.collect { case wh: INode.WhiteHole => wh }
    val holeDeps =
      whs.map { wh => wh -> bhs.filter(_.dest == wh.out) }

    s"""digraph {
graph[rankdir="BT"]
${
  iNodes.map { inode => drawNode(inodeName.id(inode.label), 'shape -> "rectangle", 'label -> inode.pretty) }.mkString("\n")
}
${
  (dataValues.keys ++ iNodes.flatMap { i => i.output }).collect { case d: DataLabel.Out => d }.toSet.map { d: DataLabel =>
    drawNode(dataName.id(d), 'label -> s"""${d.name}: ${dataValues.get(d).map(_.pretty) getOrElse "ERROR: Not Found"}""")
  }.mkString("\n")
}
${
  (iNodes.flatMap(_.effect) ++ effectDependencies.values).distinct.map { eff =>
    drawNode(effName.id(eff), 'label -> effName.name(eff))
  }.mkString("\n")
}
${
  iNodes.flatMap { inode =>
    inode.inputs.map { in =>
      drawEdge(inodeName.id(inode.label), dataName.id(dataDependencies(in)), 'label -> in.name)
    } ++ inode.output.map { out =>
      drawEdge(dataName.id(out), inodeName.id(inode.label), 'label -> out.name)
    }
  }.mkString("\n")
}
${
  effectDependencies.map { case (from, to) =>
    drawEdge(inodeName.id(from), effName.id(to))
  }.mkString("\n")
}
${
  iNodes.flatMap { inode =>
    inode.effect.map {eff => drawEdge(effName.id(eff), inodeName.id(inode.label)) }
  }.mkString("\n")
}
${
  iNodes.flatMap { inode =>
    jumpTargets(inode) map { l => drawEdge(inodeName.id(l), inodeName.id(inode.label), 'style -> "dashed") }
  }.mkString("\n")
}
${
  effectMerges.flatMap { case (m, es) =>
    es map { e => drawEdge(effName.id(m), effName.id(e), 'style -> "dotted") }
  }.mkString("\n")
}
${
  holeDeps.flatMap { case (wh, bhs) =>
    bhs map { bh => drawEdge(inodeName.id(wh.label), inodeName.id(bh.label), 'style -> "dotted") }
  }.mkString("\n")
}
}"""
    }
}
object Dataflow {
  private[this] def newMultiMap[A, B] = new mutable.HashMap[A, mutable.Set[B]] with mutable.MultiMap[A, B]

  def build(body: MethodBody): Dataflow = {
    val inodes = body.liveBytecode.collect {
      case ret :Bytecode.XReturn =>
        ret.label -> INode.XReturn()(ret.in, ret.eff)
      case iv @ Bytecode.invokevirtual(className, methodRef) =>
        iv.label -> INode.InvokeVirtual(
          className,
          methodRef,
          body.dataValue(iv.receiver),
          iv.ret.map(body.dataValue),
          iv.args.map(body.dataValue): _*
        )(
          iv.receiver,
          iv.ret,
          iv.args,
          iv.eff
        )
      case c: Bytecode.ConstX =>
        c.label -> INode.Const(c.data)(c.out)
      case j: Bytecode.Jump =>
        j.label -> INode.Goto(j.target)(j.eff)
      case cmp: Bytecode.if_icmple =>
        cmp.label -> INode.ICmp(body.dataValue(cmp.value1), body.dataValue(cmp.value2))(
          "le",
          cmp.value1,
          cmp.value2,
          cmp.target,
          cmp.eff
        )
      case iadd: Bytecode.iadd =>
        iadd.label -> INode.IAdd(body.dataValue(iadd.value1), body.dataValue(iadd.value2))(
          iadd.value1,
          iadd.value2,
          iadd.out
        )
      case unk: Bytecode.Procedure if({println(unk); false}) =>
        ???
      case unk: Bytecode.Control if({println(unk); false}) =>
        ???
      case unk: Bytecode.Control =>
        throw new AssertionError()
      case unk: Bytecode.Procedure =>
        throw new AssertionError()
      case unk if unk.effect.nonEmpty =>
        throw new AssertionError()
    }.toMap

    val inodeEffectDeps = mutable.HashMap.empty[INode.Label, Effect]
    for {
      (bcl, inode) <- inodes
      eff <- body.effectDependencies.get(bcl)
    } inodeEffectDeps(inode.label) = eff

    val bytecodeAggregate: Map[Bytecode.Label, Bytecode.Label] = {
      def f(pending: Set[Bytecode.Label], bcs: Seq[Bytecode]): Map[Bytecode.Label, Set[Bytecode.Label]] = bcs match {
        case Seq(bc: Bytecode.Shuffle, tail @ _*) => f(pending + bc.label, tail)
        case Seq(bc, tail @ _*) => Map(bc.label -> (pending + bc.label)) ++ f(Set.empty, tail)
        case Seq() => Map.empty
      }
      f(Set.empty, body.bytecode).flatMap { case(agg, ts) => ts.map(_ -> agg) }
    }

    val jumpTargets: Map[JumpTarget, INode.Label] =
      body.jumpTargets.mapValues(bytecodeAggregate).mapValues(inodes).mapValues(_.label)

    val blackHoles = body.dataMerges.flatMap { case ((pos, merged), ds) =>
      ds.map { d => (d -> INode.BlackHole(merged)) }
    }
    val whiteHoles = body.dataMerges.map { case ((pos, merged), ds) =>
      INode.WhiteHole(merged)
    }

    val holeBindings =
      blackHoles map { case (d, bh) => (bh.in -> d) }

    val holeValues = Seq.empty // blackHoles.map { case (d, bh) => bh.in -> body.dataValue(d) }

    Dataflow(
      body.isStatic,
      body.descriptor,
      inodes.values.toSeq ++ blackHoles.values ++ whiteHoles,
      body.dataBinding ++ holeBindings,
      inodeEffectDeps.toMap,
      body.dataValues ++ holeValues,
      jumpTargets,
      body.dataMerges.toMap.map { case ((pos, d), ds) => (d -> ds) },
      body.effectMerges.toMap.map { case ((pos, e), es) => (e -> es) }
    )
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

    case class XReturn()(ret: DataLabel.In, eff: Effect) extends INode {
      override val inputs = Seq(ret)
      override val output = None
      override val effect = Some(eff)
      override def pretty = "return"
    }

    case class Const(value: Data)(out: DataLabel.Out) extends INode {
      override val inputs = Seq.empty
      override val output = Some(out)
      override val effect = None
      override def pretty = s"const ${value.pretty}"
    }

    case class Goto(target: JumpTarget)(eff: Effect) extends INode {
      override val inputs = Seq.empty
      override val output = None
      override val effect = Some(eff)
      override def pretty = s"goto"
    }

    case class ICmp(value1: Data, value2: Data)(
      val op: String,
      val label1: DataLabel.In,
      val label2: DataLabel.In,
      val thenTarget: JumpTarget,
      val eff: Effect
    ) extends INode {
      override val inputs = Seq(label1, label2)
      override val output = None
      override val effect = Some(eff)
      override def pretty = s"if_icmp${op}"
    }

    case class IAdd(value1: Data, value2: Data)(
      val label1: DataLabel.In,
      val label2: DataLabel.In,
      val out: DataLabel.Out
    ) extends INode {
      override val inputs = Seq(label1, label2)
      override val output = Some(out)
      override val effect = None
      override val pretty = "iadd"
    }

    case class InvokeVirtual(
      className: ClassName,
      method: LocalMethodRef,
      receiver: Data,
      ret: Option[Data],
      args: Data*
    )(
      val receiverLabel: DataLabel.In,
      val retLabel: Option[DataLabel.Out],
      val argLabels: Seq[DataLabel.In],
      val eff: Effect
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
      override def pretty = s"WhiteHole"
    }
    case class BlackHole(dest: DataLabel.Out) extends INode {
      val in = DataLabel.in("in")
      override def inputs = Seq(in)
      override def output = None
      override val effect = Some(Effect.fresh())
      override def pretty = s"BlackHole"
    }
  }
}
