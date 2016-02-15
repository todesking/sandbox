package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod }

import com.todesking.scalapp.syntax._
sealed abstract class Bytecode {
  val label: Bytecode.Label = Bytecode.Label.fresh()
  def inputs: Seq[DataLabel.In]
  def output: Option[DataLabel.Out]
  def nextFrame(frame: Frame): FrameUpdate
  def effect: Option[Effect]
  def pretty: String = toString

  protected def update(frame: Frame): FrameUpdate =
    FrameUpdate(
      frame.copy(effect = this.effect getOrElse frame.effect),
      Map.empty,
      this.effect.map { _ => this.label -> frame.effect }.toMap,
      Map.empty
    )
}
object Bytecode {
  class Label extends AbstractLabel
  object Label extends AbstractLabel.NamerProvider[Label] {
    def fresh(): Label = new Label
  }

  def load(t: TypeRef, n: Int): Bytecode =
    t match {
      case TypeRef.Int => iload(n)
      case TypeRef.Reference(_) => aload(n)
      case TypeRef.This => aload(n)
      case unk =>
        throw new IllegalArgumentException(s"Unsupported load instruction for ${unk}")
    }

  def store(t: TypeRef, n: Int): Bytecode =
    t match {
      case TypeRef.Int => istore(n)
      case TypeRef.This => astore(n)
      case unk =>
        throw new IllegalArgumentException(s"Unsupported store instruction for ${unk}")
    }

  sealed abstract class Control extends Bytecode {
    val eff: Effect = Effect.fresh()
    override def effect = Some(eff)
  }
  sealed abstract class Shuffle extends Bytecode {
    override def inputs = Seq.empty
    override def output = None
    override def effect = None
  }
  sealed abstract class Procedure extends Bytecode

  sealed abstract class Jump extends Control {
    override def inputs = Seq.empty
    override def output = None
    override def nextFrame(f: Frame) = update(f)
    def target: JumpTarget
  }

  sealed abstract class Branch extends Control {
    def target: JumpTarget
  }

  sealed abstract class Return extends Control {
  }
  sealed abstract class XReturn extends Return {
    val in: DataLabel.In = DataLabel.in("retval")
    override val inputs = Seq(in)
    override def output = None
    override def nextFrame(f: Frame) = update(f).ret(in)
  }

  sealed abstract class if_icmpXX extends Branch {
    val value1: DataLabel.In = DataLabel.in("value1")
    val value2: DataLabel.In = DataLabel.in("value2")
    override def inputs = Seq(value1, value2)
    override def output = None
    override def nextFrame(f: Frame) = update(f).pop1(value2).pop1(value1)
  }

  sealed abstract class Load1 extends Shuffle {
    def n: Int
    override def nextFrame(f: Frame) = update(f).load1(n)
  }

  sealed abstract class Store1 extends Shuffle {
    def n: Int
    override def nextFrame(f: Frame) = update(f).store1(n)
  }

  sealed abstract class ConstX extends Procedure {
    def out: DataLabel.Out
    def data: Data
    override def inputs = Seq.empty
    override def output = Some(out)
    override def effect = None
  }

  sealed abstract class Const1 extends ConstX {
    final val out: DataLabel.Out = DataLabel.out("const(1word)")
    override def nextFrame(f: Frame) = update(f).push1(out -> data)
  }

  sealed abstract class Const2 extends ConstX {
    final val out: DataLabel.Out = DataLabel.out("const(2word)")
    override def nextFrame(f: Frame) = update(f).push2(out -> data)
  }

  case class nop() extends Shuffle {
    override def nextFrame(f: Frame) = update(f)
  }
  case class dup() extends Shuffle {
    override def nextFrame(f: Frame) = update(f).push(f.stack.head)
  }
  case class pop() extends Shuffle {
    override def nextFrame(f: Frame) = update(f).pop1()
  }
  case class iload(n: Int) extends Load1
  case class aload(n: Int) extends Load1
  case class istore(n: Int) extends Store1
  case class astore(n: Int) extends Store1
  case class ireturn() extends XReturn
  case class lreturn() extends XReturn
  case class iconst(value: Int) extends Const1 {
    override def data = Data(TypeRef.Int, Some(value))
  }
  case class lconst(value: Long) extends Const2 {
    override def data = Data(TypeRef.Long, Some(value))
  }
  case class aconst_null() extends Const1 {
    override def data = Data(TypeRef.Null, Some(null))
  }
  case class goto(override val target: JumpTarget) extends Jump {
  }
  case class if_icmple(override val target: JumpTarget) extends if_icmpXX
  case class iadd() extends Procedure {
    val value1 = DataLabel.in("value1")
    val value2 = DataLabel.in("value2")
    val out = DataLabel.out("result")
    override def inputs = Seq(value1, value2)
    override def output = Some(out)
    override def effect = None
    override def nextFrame(f: Frame) =
      (f.stack(0), f.stack(1)) match {
        case ((_, Data(TypeRef.Int, v1)), (_, Data(TypeRef.Int, v2))) =>
          update(f).pop1(value2).pop1(value1).push(out -> Data(TypeRef.Int, v1.flatMap { v1 => v2.map { v2 => v1.asInstanceOf[Int] + v2.asInstanceOf[Int] }}))
        case (d1, d2) => throw new IllegalArgumentException(s"Type error: ${(d1, d2)}")
      }
  }
  case class invokevirtual(classRef: ClassRef, methodRef: LocalMethodRef) extends Procedure {
    override def pretty = s"invokevirtual ${classRef.str}.${methodRef.str}"
    override def effect = Some(eff)
    val eff: Effect = Effect.fresh()
    val receiver: DataLabel.In = DataLabel.in("receiver")
    val args: Seq[DataLabel.In] = methodRef.args.zipWithIndex.map { case (_, i) => DataLabel.in(s"arg${i}") }
    val ret: Option[DataLabel.Out] = if(methodRef.isVoid) None else Some(DataLabel.out("ret"))
    override def inputs = receiver +: args
    override def output = ret
    override def nextFrame(f: Frame) = {
      require(f.stack.size >= methodRef.args.size)
      val popped =
        args.zip(methodRef.args).foldRight(update(f)) { case ((a, t), u) =>
          if(t.isDoubleWord) u.pop2(a)
          else u.pop1(a)
        }.pop1(receiver)
      ret.fold(popped) { rlabel => popped.push(rlabel -> Data(methodRef.ret, None)) }
    }
  }
}
