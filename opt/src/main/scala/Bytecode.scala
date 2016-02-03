package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod }

import com.todesking.scalapp.syntax._
sealed abstract class Bytecode {
  val label: Bytecode.Label = Bytecode.Label.fresh()
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

  sealed abstract class Jump extends Bytecode {
    def target: JumpTarget
  }
  sealed abstract class Branch extends Bytecode {
    def target: JumpTarget
  }

  case class nop() extends Bytecode
  case class iload(n: Int) extends Bytecode
  case class aload(n: Int) extends Bytecode
  case class istore(n: Int) extends Bytecode
  case class ireturn() extends Bytecode
  case class lreturn() extends Bytecode
  case class iconst(value: Int) extends Bytecode
  case class lconst(value: Long) extends Bytecode
  case class aconst_null() extends Bytecode
  case class goto(override val target: JumpTarget) extends Jump
  case class if_icmple(override val target: JumpTarget) extends Branch
  case class invokevirtual(className: ClassName, methodRef: LocalMethodRef) extends Bytecode
}
