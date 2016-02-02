package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{Method => JMethod}

import com.todesking.scalapp.syntax._

// NOTE: Equality is defined without label
sealed abstract class Instruction {
  final val label = InstructionLabel.fresh()
  def nextFrame(frame: Frame): FrameUpdate
  def updateValues(f: DataLabel.In => Data): Map[DataLabel.Out, Data]
  val output: Option[DataLabel.Out]
  val inputs: Seq[DataLabel.In]
}
object Instruction {
  case class Return() extends Instruction {
    val retVal: DataLabel.In = DataLabel.in("retval")
    override val inputs = Seq(retVal)
    override val output = None
    override def nextFrame(frame: Frame) =
      FrameUpdate(Frame.empty, Seq(frame.stack(0) -> retVal))
    override def updateValues(f: DataLabel.In => Data) = Map.empty
  }

  case class Const(tpe: TypeRef, value: Any) extends Instruction {
    val valueLabel = DataLabel.out("const")
    override val output = Some(valueLabel)
    override val inputs = Seq.empty
    override def nextFrame(frame: Frame) =
      FrameUpdate(frame.pushStack(valueLabel))
    override def updateValues(f: DataLabel.In => Data) =
      Map(valueLabel -> Data(tpe, Some(value)))
  }

  case class InvokeVirtual(className: ClassName, methodRef: LocalMethodRef) extends Instruction {
    override val inputs = (0 to methodRef.descriptor.args.size).map { i => DataLabel.in(if(i == 0) "receiver" else s"arg_${i}") }.toSeq
    override val output = if(methodRef.isVoid) None else Some(DataLabel.out("ret"))
    val argAndType: Seq[(DataLabel.In, TypeRef.Public)] = inputs.zip(TypeRef.Reference(className) +: methodRef.descriptor.args)
    override def nextFrame(frame: Frame) =
      FrameUpdate(
        output.map { o =>
          frame.dropStack(methodRef.descriptor.args.size + 1).pushStack(o)
        } getOrElse {
          frame.dropStack(methodRef.descriptor.args.size + 1)
        },
        dataIn = frame.takeStack(methodRef.descriptor.args.size + 1).reverse.zip(inputs)
      )
    override def updateValues(f: DataLabel.In => Data) =
      output.map(_ -> Data(methodRef.descriptor.ret, None)).toMap
  }

  case class IfICmpLE(thenTarget: JumpTarget, elseTarget: JumpTarget) extends Instruction {
    val value1 =DataLabel.in("value1")
    val value2 =DataLabel.in("value2")
    override val output = None
    override val inputs = Seq(value2, value1)
    override def nextFrame(frame: Frame) =
      FrameUpdate(
        frame.dropStack(2),
        frame.takeStack(2).zip(inputs)
      )
    override def updateValues(f: DataLabel.In => Data) =
      Map.empty
  }

  case class Goto(target: JumpTarget) extends Instruction {
    override val output = None
    override val inputs = Seq.empty
    override def nextFrame(frame: Frame) =
      FrameUpdate(frame)
    override def updateValues(f: DataLabel.In => Data) =
      Map.empty
  }
}

