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
  startINodeLabel: Dataflow.INode.Label,
  iNodes: Seq[Dataflow.INode],
  dataBinding: Map[DataLabel.In, DataLabel.Out],
  dataValues: Map[DataLabel, Data],
  jumpTargets: Map[JumpTarget, Dataflow.INode.Label],
  fallThroughs: Map[Dataflow.INode.Label, Dataflow.INode.Label],
  dataMerges: Map[DataLabel.Out, Set[DataLabel.Out]]
) {
  require(iNodes.nonEmpty)

  import Dataflow.INode

  def data(l: DataLabel): Data = dataValues(l)

  private[this] def sortINode() = {
    val tails = fallThroughs.values.toSet

    def dragOut(start: INode.Label, blob: Set[INode.Label]): (Seq[INode.Label], Set[INode.Label]) = {
      fallThroughs.get(start).fold(Seq(start) -> blob) { next =>
        val (s, b) = dragOut(next, blob - next)
        (start +: s) -> b
      }
    }

    def sort(start: INode.Label, blob: Set[INode.Label]): Seq[INode.Label] = {
      val (s, b) = dragOut(start, blob)
      startingPoint(b).fold(s) { next =>
        s ++ sort(next, b - next)
      }
    }

    def startingPoint(blob: Set[INode.Label]): Option[INode.Label] = {
      if(blob.isEmpty) {
        None
      } else {
        blob.find { l => !tails.contains(l) }.fold(throw new AssertionError) { l => Some(l) }
      }
    }

    sort(startINodeLabel, iNodes.map(_.label).toSet - startINodeLabel)
  }

  def compile(): MethodBody = {
    val bcs = mutable.ArrayBuffer.empty[Bytecode]

    val sorted = sortINode().map { l => iNodes.find(_.label == l).get }

    val predefinedLocalSize = descriptor.args.size + (if(isStatic) 0 else 1)

    val localAssigns: Map[DataLabel.In, Int] = dataBinding.keys.zipWithIndex.map { case (in, i) =>
      in -> ( i + predefinedLocalSize)
    }.toMap

    def genLoad(i: DataLabel.In): Seq[Bytecode] =
      Seq(Bytecode.load(dataValues(i).typeRef, localAssigns(i)))

    def genStores(o: DataLabel.Out): Seq[Bytecode] = {
      val stores =
        dataBinding
          .filter(_._2 == o)
          .map(_._1)
          .map(localAssigns).map { n =>
            Bytecode.store(dataValues(o).typeRef, n)
          }
      if(stores.size < 2) {
        stores.toSeq
      } else {
        stores
          .zip(
            Stream.fill(stores.size - 1)(Seq(Bytecode.dup())) ++ Seq(Seq())
          ).flatMap { case (s, d) => s +: d }.toSeq
      }
    }

    import INode._
    import Bytecode._
    val bytecode: Seq[(INode, Seq[Bytecode])] =
      sorted.map {
        case i @ XReturn() =>
          descriptor.ret match {
            case TypeRef.Int =>
              i -> Seq(
                iload(localAssigns(i.ret)),
                ireturn()
              )
            case _ => ???
          }
        case i @ Const(data) =>
          data.typeRef match {
            case TypeRef.Int =>
              i ->Seq(
                Seq(iconst(data.value.get.asInstanceOf[Int])),
                genStores(i.out)
              ).flatten
            case _ => ???
          }
        case i :InvokeVirtual =>
          i -> Seq(
            (i.receiverLabel +: i.argLabels).flatMap(genLoad),
            Seq(invokevirtual(i.className, i.method)),
            i.retLabel.toSeq.flatMap(genStores)
          ).flatten
        case unk =>
          throw new NotImplementedError(unk.toString)
      }

    iNodes.toVector.pp()
    sorted.pp()

    val jts = jumpTargets.map { case (jt, iLabel) =>
      bytecode.find(_._1.label == iLabel).map(jt -> _._2.head.label).get
    }

    MethodBody(
      isStatic,
      descriptor,
      bytecode.flatMap(_._2),
      jts,
      100, // TODO
      100
    )
  }

  private[this] val out2inode = iNodes.flatMap { inode => inode.output.map { o => (o -> inode) } }.toMap

  def startINode: INode = iNodes.find(_.label == startINodeLabel).get

  def iNode(l: INode.Label): INode =
    iNodes.find(_.label == l).get

  def dependentINodes(inode: INode): Seq[INode] =
    inode.inputs.map(dataBinding).flatMap(out2inode.get)

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

    s"""digraph {
graph[rankdir="BT"]
start[label="start" shape="doublecircle"]
${inodeName.id(startINode.label)} -> start
${
  iNodes.map { inode => drawNode(inodeName.id(inode.label), 'shape -> "rectangle", 'label -> inode.pretty) }.mkString("\n")
}
${
  (dataValues.keys ++ iNodes.flatMap { i => i.output }).collect { case d: DataLabel.Out => d }.toSet.map { d: DataLabel =>
    drawNode(dataName.id(d), 'label -> s"""${d.name}: ${dataValues.get(d).map(_.pretty) getOrElse "ERROR: Not Found"}""")
  }.mkString("\n")
}
${
  iNodes.flatMap { inode =>
    inode.inputs.map { in =>
      drawEdge(inodeName.id(inode.label), dataName.id(dataBinding(in)), 'label -> in.name, 'style -> "dashed")
    } ++ inode.output.map { out =>
      drawEdge(dataName.id(out), inodeName.id(inode.label), 'label -> out.name, 'style -> "dashed")
    }
  }.mkString("\n")
}
${
  iNodes.flatMap { inode =>
    jumpTargets(inode) map { l => drawEdge(inodeName.id(l), inodeName.id(inode.label), 'label -> "then", 'style -> "") }
  }.mkString("\n")
}
${
  iNodes.flatMap { inode =>
    fallThroughs.get(inode.label) map { l => drawEdge(inodeName.id(l), inodeName.id(inode.label), 'style -> "") }
  }.mkString("\n")
}
${
  dataMerges.flatMap { case (m, ds) =>
    ds.map { d => drawEdge(dataName.id(m), dataName.id(d), 'style -> "dotted") }
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

    // TODO: insert goto at BB end

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

    val fallThroughs = body.fallThroughs
      .map { case (from, to) => inodes(bytecodeAggregate(from)) -> inodes(bytecodeAggregate(to)) }
      .filter { case (from, to) => from.label != to.label}
      .map { case (from, to) => (from.label, to.label) }
      .toMap

    Dataflow(
      body.isStatic,
      body.descriptor,
      inodes(bytecodeAggregate(body.bytecode.head.label)).label,
      inodes.values.toSeq,
      body.dataBinding,
      body.dataValues,
      jumpTargets,
      fallThroughs,
      body.dataMerges
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

    case class XReturn()(val ret: DataLabel.In, val eff: Effect) extends INode {
      override val inputs = Seq(ret)
      override val output = None
      override val effect = Some(eff)
      override def pretty = "return"
    }

    case class Const(value: Data)(val out: DataLabel.Out) extends INode {
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

      override def inputs = receiverLabel +: argLabels
      override def output = retLabel
      override def effect = Some(eff)
      override def pretty = s"""invokevirtual ${className.binaryString}#${method.str}"""
    }
  }
}
