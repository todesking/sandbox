package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod }

import com.todesking.scalapp.syntax._

case class MethodBody(
    descriptor: MethodDescriptor,
    attribute: MethodAttribute,
    bytecode: Seq[Bytecode],
    jumpTargets: Map[JumpTarget, Bytecode.Label]
) {
  require(bytecode.nonEmpty)

  def isStatic: Boolean = attribute.isStatic

  // TODO: Exception handler

  def methodReferences: Set[(ClassRef, MethodRef)] =
    bytecode.collect { case bc: Bytecode.HasMethodRef => (bc.classRef -> bc.methodRef) }.toSet

  def fieldReferences: Set[(ClassRef, FieldRef)] =
    bytecode.collect { case bc: Bytecode.HasFieldRef => (bc.classRef -> bc.fieldRef) }.toSet

  def labelToBytecode(l: Bytecode.Label): Bytecode =
    bytecode.find(_.label == l).getOrElse { throw new IllegalArgumentException(s"Bytecode ${l} not found") }

  def rewrite(f: PartialFunction[Bytecode, Bytecode]): MethodBody = {
    val lifted = f.lift
    bytecode.foldLeft(this) {
      case (body, bc) =>
        val newBc = lifted(bc) getOrElse bc
        body.replaceBytecode(bc.label, newBc)
    }
  }

  def rewriteClassRef(from: ClassRef, to: ClassRef): MethodBody = {
    rewrite { case bc: Bytecode.HasClassRef if bc.classRef == from => bc.rewriteClassRef(to) }
  }

  def replaceBytecode(l: Bytecode.Label, newBc: Bytecode): MethodBody = {
    if (newBc.label == l) {
      this
    } else {
      val newBcs = bytecode.map { bc => if (bc.label == l) newBc else bc }
      val newJts = jumpTargets.map { case (jt, bcl) => if (bcl == l) (jt -> newBc.label) else (jt -> bcl) }
      MethodBody(descriptor, attribute, newBcs, newJts)
    }
  }

  def pretty: String = {
    val lName = Bytecode.Label.namer("L", "")
    s"""${descriptor.str} ${attribute}
${
      bytecode.map { bc =>
        val l = f"L${bc.label.innerId}%-5s "
        l + (bc match {
          case j: Bytecode.Jump =>
            s"${j} # L${jumpTargets(j.target).innerId}"
          case b: Bytecode.Branch =>
            s"${b} # L${jumpTargets(b.target).innerId}"
          case b =>
            b.pretty
        })
      }.mkString("\n")
    }
"""
  }

  def dataflow(self: Instance[_ <: AnyRef]): MethodBody.DataFlow =
    new MethodBody.DataFlow(this, Data.Reference(self.thisRef.toTypeRef, self))
}

object MethodBody {
  def parse(m: JMethod): Option[MethodBody] =
    Javassist.decompile(m)

  class DataFlow(val body: MethodBody, self: Data) {
    def possibleValues(l: DataLabel): Seq[Data] = l match {
      case l: DataLabel.Out =>
        dataMerges.get(l) map { ms =>
          // TODO: Is this cause infinite loop?
          ms.toSeq.flatMap { m => possibleValues(m) }
        } getOrElse {
          Seq(dataValue(l))
        }
      case l: DataLabel.In =>
        possibleValues(dataBinding(l))
    }

    def onlyValue(l: DataLabel): Option[Data] = {
      val pvs = possibleValues(l)
      if(pvs.size == 1) Some(pvs.head)
      else None
    }

    def argLabels: Seq[DataLabel.Out] =
      body.descriptor.args
        .zipWithIndex
        .map { case (t, i) => DataLabel.out(s"arg_${i}") }

    lazy val initialFrame: Frame = {
      val initialEffect = Effect.fresh()
      val thisData = if (body.isStatic) None else Some(DataLabel.out("this") -> self)
      val argData = body.descriptor.args.zipWithIndex.zip(argLabels).flatMap {
        case ((t, i), label) =>
          val data = Data.Unsure(t)
          if (t.isDoubleWord)
            Seq((DataLabel.out(s"second word of ${label.name}") -> data.secondWordData), (label -> data))
          else
            Seq(label -> data)
      }
      Frame((thisData.toSeq ++ argData).zipWithIndex.map(_.swap).toMap, List.empty, initialEffect)
    }
    def dataValue(l: DataLabel): Data =
      dataValues(l)

    def dataType(l: DataLabel): TypeRef = dataValues(l).typeRef

    def toDot(): String = {
      import Graphviz._
      val bcs = body.bytecode.map { bc => (bc.label -> bc) }.toMap
      val bcName = Bytecode.Label.namer("bytecode_", "")
      val dName = DataLabel.namer("data_", "")
      val eName = Effect.namer("effect_", "Eff#")
      s"""digraph {
  graph[rankdir="BT"]
  start[label="start" shape="doublecircle"]
  ${bcName.id(body.bytecode.head.label)} -> start
  ${eName.id(initialFrame.effect)} -> start [style="dotted"]
      ${
        body.bytecode.map { bc =>
          drawNode(bcName.id(bc.label), 'label -> bc.pretty, 'shape -> "rectangle")
        }.mkString("\n")
      }
      ${
        fallThroughs.map {
          case (src, d) =>
            drawEdge(bcName.id(d), bcName.id(src))
        }.mkString("\n")
      }
      ${
        body.bytecode.flatMap {
          case bc: Bytecode.Jump =>
            Seq(drawEdge(bcName.id(body.jumpTargets(bc.target)), bcName.id(bc.label)))
          case bc: Bytecode.Branch =>
            Seq(drawEdge(bcName.id(body.jumpTargets(bc.target)), bcName.id(bc.label), 'label -> "then"))
          case _ =>
            Seq.empty
        }.mkString("\n")
      }
      ${
        dataValues.collect {
          case (l: DataLabel.Out, data) =>
            drawNode(dName.id(l), 'label -> s"${l.name}: ${data.pretty}")
        }.mkString("\n")
      }
      ${
        body.bytecode.flatMap { bc =>
          bc.inputs.flatMap { i =>
            dataBinding.get(i).map(i -> _)
          }.map {
            case (i, o) =>
              drawEdge(bcName.id(bc.label), dName.id(o), 'style -> "dotted", 'label -> i.name)
          }
        }.mkString("\n")
      }
      ${
        body.bytecode.flatMap { bc => bc.output.map(bc -> _) }.map {
          case (bc, o) =>
            drawEdge(dName.id(o), bcName.id(bc.label), 'style -> "dotted", 'label -> o.name)
        }.mkString("\n")
      }
      ${
        dataMerges.flatMap {
          case (m, ds) =>
            ds.map { d => drawEdge(dName.id(m), dName.id(d), 'style -> "dotted") }
        }.mkString("\n")
      }
      ${
        effectMerges.flatMap {
          case (m, es) =>
            es.map { e => drawEdge(eName.id(m), eName.id(e), 'style -> "dotted") }
        }.mkString("\n")
      }
      ${
        effectDependencies.map {
          case (bcl, e) =>
            drawEdge(bcName.id(bcl), eName.id(e), 'style -> "dotted")
        }.mkString("\n")
      }
      ${
        body.bytecode.flatMap { bc => bc.effect.map(bc -> _) }.map {
          case (bc, e) =>
            drawEdge(eName.id(e), bcName.id(bc.label), 'style -> "dotted")
        }.mkString("\n")
      }
  }"""
    }

    // Yes I know this is just a pattern matching, not type-annotation. But I need readability
    lazy val (
      dataBinding: Map[DataLabel.In, DataLabel.Out],
      dataValues: Map[DataLabel, Data],
      dataMerges: Map[DataLabel.Out, Set[DataLabel.Out]],
      effectDependencies: Map[Bytecode.Label, Effect],
      effectMerges: Map[Effect, Set[Effect]],
      liveBytecode: Seq[Bytecode],
      fallThroughs: Map[Bytecode.Label, Bytecode.Label],
      maxLocals: Int,
      maxStackDepth: Int
      ) = {
      val dataMerges = new AbstractLabel.Merger[DataLabel.Out](DataLabel.out("merged"))
      val effectMerges = new AbstractLabel.Merger[Effect](Effect.fresh())
      def mergeData(d1: (DataLabel.Out, Data), d2: (DataLabel.Out, Data)): (DataLabel.Out, Data) =
        (dataMerges.merge(d1._1, d2._1) -> Data.merge(d1._2, d2._2))
      def merge(f1: Frame, f2: Frame): Frame = {
        Frame(
          (f1.locals.keySet ++ f2.locals.keySet)
          .filter { k => f1.locals.contains(k) && f2.locals.contains(k) }
          .map { k => (k -> mergeData(f1.locals(k), f2.locals(k))) }.toMap,
          f1.stack.zip(f2.stack).map { case (a, b) => mergeData(a, b) },
          effectMerges.merge(f1.effect, f2.effect)
        )
      }

      val preFrames = mutable.HashMap.empty[Bytecode.Label, Frame]
      val updates = mutable.HashMap.empty[Bytecode.Label, FrameUpdate]
      val falls = mutable.HashMap.empty[Bytecode.Label, Bytecode.Label]
      val fallThroughs = new mutable.HashMap[Bytecode.Label, Bytecode.Label]

      val liveBcs = mutable.HashMap.empty[Bytecode.Label, Bytecode]

      val tasks = mutable.Set.empty[(Bytecode.Label, Frame)]
      tasks += (body.bytecode.head.label -> initialFrame)

      while (tasks.nonEmpty) {
        val (pos, frame) = tasks.head
        tasks.remove(pos -> frame)
        val merged = preFrames.get(pos).map(merge(_, frame)) getOrElse frame
        if (preFrames.get(pos).map(_ != merged) getOrElse true) {
          preFrames(pos) = merged
          val bseq = body.bytecode.dropWhile(_.label != pos)
          val bc = bseq.head
          liveBcs(bc.label) = bc
          val u = bc.nextFrame(merged)
          updates(bc.label) = u
          bc match {
            case r: Bytecode.VoidReturn =>
            case r: Bytecode.XReturn =>
            case j: Bytecode.Jump =>
              tasks += (body.jumpTargets(j.target) -> u.newFrame)
            case b: Bytecode.Branch =>
              tasks += (body.jumpTargets(b.target) -> u.newFrame)
              tasks += (bseq(1).label -> u.newFrame)
              fallThroughs(b.label) = bseq(1).label
            case _: Bytecode.Procedure | _: Bytecode.Shuffle =>
              tasks += (bseq(1).label -> u.newFrame)
              fallThroughs(bc.label) = bseq(1).label
            case Bytecode.athrow() =>
            // TODO: Exception handler
          }
        }
      }

      val dataValues = mutable.HashMap.empty[DataLabel, Data]
      (preFrames.values.toSeq :+ initialFrame) foreach { frame =>
        (frame.locals.values ++ frame.stack) foreach {
          case (l, d) =>
            dataValues(l) = d
        }
      }
      val binding = mutable.HashMap.empty[DataLabel.In, DataLabel.Out]
      val effectDependencies = mutable.HashMap.empty[Bytecode.Label, Effect]
      updates.values foreach { u =>
        dataValues ++= u.dataValues
        binding ++= u.binding
        effectDependencies ++= u.effectDependencies
      }

      val allFrames = preFrames.values ++ updates.values.map(_.newFrame)
      val maxLocals = allFrames.flatMap(_.locals.keys).max + 1
      val maxStackDepth = allFrames.map(_.stack.size).max

      (binding.toMap, dataValues.toMap, dataMerges.toMap, effectDependencies.toMap, effectMerges.toMap, liveBcs.values.toSeq, fallThroughs.toMap, maxLocals, maxStackDepth)
    }
  }
}

