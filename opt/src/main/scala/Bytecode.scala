package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod }

import com.todesking.scalapp.syntax._
sealed abstract class Bytecode {
  val label: Bytecode.Label = Bytecode.Label.fresh()
  def nextFrame(frame: Frame): FrameUpdate
}
object Bytecode {
  class Label extends AbstractLabel
  object Label {
    def fresh(): Label = new Label
  }

  def load(t: TypeRef, n: Int): Bytecode =
    t match {
      case TypeRef.Int => iload(n)
      case TypeRef.Reference(_) => aload(n)
      case unk =>
        throw new IllegalArgumentException(s"Unsupported load instruction for ${unk}")
    }

  def store(t: TypeRef, n: Int): Bytecode =
    t match {
      case TypeRef.Int => istore(n)
      case unk =>
        throw new IllegalArgumentException(s"Unsupported store instruction for ${unk}")
    }

  sealed abstract class Control extends Bytecode {
  }

  sealed abstract class Jump extends Control {
    def target: JumpTarget
  }
  sealed abstract class Branch extends Jump {
  }
  sealed abstract class Return extends Control {
    override def nextFrame(f: Frame) = f.update.ret
  }
  sealed abstract class if_icmpXX extends Branch {
    val value1: DataLabel.In = DataLabel.in("value1")
    val value2: DataLabel.In = DataLabel.in("value2")
    override def nextFrame(f: Frame) = f.update.pop1(value1).pop1(value2)
  }

  sealed abstract class Load1 extends Bytecode {
    def n: Int
    override def nextFrame(f: Frame) = f.update.load1(n)
  }
  sealed abstract class Store1 extends Bytecode {
    def n: Int
    override def nextFrame(f: Frame) = f.update.store1(n)
  }
  sealed abstract class Const1 extends Bytecode {
    final val outLabel: DataLabel.Out = DataLabel.out("const")
    override def nextFrame(f: Frame) = f.update.push1(outLabel)
  }
  sealed abstract class Const2 extends Bytecode {
    final val outLabel: DataLabel.Out = DataLabel.out("const")
    override def nextFrame(f: Frame) = f.update.push2(outLabel)
  }

  case class nop() extends Bytecode {
    override def nextFrame(f: Frame) = f.update
  }
  case class iload(n: Int) extends Load1
  case class aload(n: Int) extends Load1
  case class istore(n: Int) extends Store1
  case class ireturn() extends Return
  case class lreturn() extends Return
  case class iconst(value: Int) extends Const1
  case class lconst(value: Long) extends Const2
  case class aconst_null() extends Const1
  case class goto(override val target: JumpTarget) extends Jump {
    override def nextFrame(f: Frame) = f.update
  }
  case class if_icmple(override val target: JumpTarget) extends if_icmpXX
  case class invokevirtual(className: ClassName, methodRef: LocalMethodRef) extends Bytecode {
    val receiver: DataLabel.In = DataLabel.in("receiver")
    val args: Seq[DataLabel.In] = methodRef.args.zipWithIndex.map { case (_, i) => DataLabel.in(s"arg${i}") }
    val ret: Option[DataLabel.Out] = if(methodRef.isVoid) None else Some(DataLabel.out("ret"))
    override def nextFrame(f: Frame) = {
      require(f.stack.size >= methodRef.args.size)
      args.zip(methodRef.args).foldRight(f.update) { case ((a, t), u) =>
        if(t.isDoubleWord) u.pop2(a)
        else u.pop1(a)
      }.pop1(receiver)
    }
  }
}
