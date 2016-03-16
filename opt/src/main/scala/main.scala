package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

import java.lang.reflect.{ Method => JMethod, Field => JField, Modifier }

import com.todesking.scalapp.syntax._

object Main {
  def main(args: Array[String]): Unit = {
  }
}

object Opt {
  def optimize[A <: AnyRef: ClassTag](orig: A): A = {
    val instance = Instance.of(orig)
    instance.value
  }
}

abstract class TransformListener {
  def stage[A](name: String)(f: TransformListener => A): A
  def process[A](cr: ClassRef, fr: FieldRef)(f: TransformListener => A): A
  def process[A](cr: ClassRef, mr: MethodRef)(f: TransformListener => A): A
  def process[A](instance: Instance[_])(f: TransformListener => A): A
  def log(s: String): Unit
}

object Log {
  def start(s: String): Unit = {
    println()
    println(s"===== START $s =====")
  }
  def end(s: String): Unit = {
    println(s"^^^^^ END $s ^^^^^")
  }
  def classAndMethods(name: String, ms: Iterable[(ClassRef, MethodRef)]): Unit = {
    start(name)
    ms.foreach { case (cr, mr) =>
      println(s"[C] $cr")
      println(s"  [M] .$mr")
    }
    end(name)
  }
  def methods(name: String, ms: Iterable[MethodRef]): Unit = {
    start(name)
    ms.foreach { case mr =>
      println(s"[M] $mr")
    }
    end(name)
  }
  def failure(transformerName: String, e: Exception): Unit = {
    println(s"==== ERROR at transformer $transformerName")
    e match {
    case e: UnveilException.HasMethodBody =>
      println(e)
      println(e.methodBody.pretty)
    case e =>
      println(e)
    }
  }
}

// TODO: add query methods about types(isDoubleWord etc) for FrameUpdate
case class FrameItem(label: DataLabel.Out, data: Data, placedBy: Option[Bytecode.Label])

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
    override def toString = s"DataLabel.In(${name})#${innerId}"
  }
  final class Out(name: String) extends DataLabel(name) {
    override def toString = s"DataLabel.Out(${name})#${innerId}"
  }

  def in(name: String) = new In(name)
  def out(name: String) = new Out(name)
}

final class Effect private extends AbstractLabel
object Effect extends AbstractLabel.NamerProvider[Effect] {
  def fresh() = new Effect
}

