package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import java.lang.reflect.{ Method => JMethod }

import com.todesking.scalapp.syntax._

case class FrameUpdate(
  newFrame: Frame,
  binding: Map[DataLabel.In, DataLabel.Out],
  effectDependencies: Map[Bytecode.Label, Effect],
  dataValues: Map[DataLabel, Data]
) {
  private[this] def requireSecondWord(ld: (DataLabel.Out, Data)): Unit =
    if(ld._2.typeRef != TypeRef.SecondWord)
      throw new RuntimeException(s"second word expected but ${ld}")

  private[this] def requireSingleWord(ld: (DataLabel.Out, Data)): Unit =
    if(ld._2.typeRef.isDoubleWord || ld._2.typeRef == TypeRef.SecondWord || ld._2.typeRef == TypeRef.Undefined)
      throw new RuntimeException(s"single word expected but ${ld}")

  private[this] def requireDoubleWord(ld: (DataLabel.Out, Data)): Unit =
    if(!ld._2.typeRef.isDoubleWord || ld._2.typeRef == TypeRef.SecondWord || ld._2.typeRef == TypeRef.Undefined)
      throw new RuntimeException(s"double word expected but ${ld}")

  private[this] def makeSecondWord(d: (DataLabel.Out, Data)): (DataLabel.Out, Data) =
    (DataLabel.out(s"second word of ${d._1.name}") -> d._2.secondWordData)

  def pop1(in: DataLabel.In): FrameUpdate = {
    requireSingleWord(newFrame.stackTop)
    val (out, d) = newFrame.stackTop
    pop0(in, out, d, newFrame.stack.drop(1))
  }

  def pop2(in: DataLabel.In): FrameUpdate = {
    requireSecondWord(newFrame.stack(0))
    requireDoubleWord(newFrame.stack(1))
    val (out, d) = newFrame.stack(1)
    pop0(in, out, d, newFrame.stack.drop(2))
  }

  private[this] def pop0(in: DataLabel.In, out: DataLabel.Out, data: Data, stack: List[(DataLabel.Out, Data)]): FrameUpdate =
    FrameUpdate(
      newFrame.copy(stack = stack),
      binding + (in -> out),
      effectDependencies,
      dataValues + (in -> data)
    )

  def push(d: (DataLabel.Out, Data)): FrameUpdate =
    if(d._2.typeRef.isDoubleWord) push2(d)
    else push1(d)

  def push1(d: (DataLabel.Out, Data)): FrameUpdate = {
    requireSingleWord(d)
    push0(d, d :: newFrame.stack)
  }

  def push2(d: (DataLabel.Out, Data)): FrameUpdate = {
    requireDoubleWord(d)
    push0(d, makeSecondWord(d) :: d :: newFrame.stack)
  }

  private[this] def push0(d: (DataLabel.Out, Data), stack: List[(DataLabel.Out, Data)]): FrameUpdate =
    FrameUpdate(
      newFrame.copy(stack = stack),
      binding,
      effectDependencies,
      dataValues + d
    )

  def setLocal(n: Int, data: (DataLabel.Out, Data)): FrameUpdate = {
    val locals =
      if(data._2.typeRef.isDoubleWord)
        newFrame.locals.updated(n, data).updated(n + 1, makeSecondWord(data))
      else
        newFrame.locals.updated(n, data)
    FrameUpdate(newFrame.copy(locals = newFrame.locals.updated(n, data)), binding, effectDependencies, dataValues)
  }

  private[this] def local1(n: Int): (DataLabel.Out, Data) = {
    requireSingleWord(newFrame.locals(n))
    newFrame.locals(n)
  }

  private[this] def local2(n: Int): (DataLabel.Out, Data) = {
    requireDoubleWord(newFrame.locals(n))
    requireSecondWord(newFrame.locals(n + 1))
    newFrame.locals(n)
  }

  def load1(n: Int): FrameUpdate = push1(local1(n))
  def load2(n: Int): FrameUpdate = push2(local2(n))

  def store1(n: Int): FrameUpdate = {
    requireSingleWord(newFrame.stackTop)
    setLocal(n, newFrame.stackTop)
  }

  def ret(retval: DataLabel.In): FrameUpdate = {
    val (l, d) =
      if(newFrame.stackTop._2.typeRef == TypeRef.SecondWord) newFrame.stack(1)
      else newFrame.stackTop
    FrameUpdate(
      Frame(Map.empty, List.empty, newFrame.effect),
      binding + (retval -> l),
      effectDependencies,
      dataValues + (retval -> d)
    )
  }
}
