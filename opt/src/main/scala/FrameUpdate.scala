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
  binding: Map[DataLabel.In, DataLabel.Out]
) {
  def pop1(l: DataLabel.In): FrameUpdate =
    FrameUpdate(
      newFrame.copy(stack = newFrame.stack.tail),
      binding + (l -> newFrame.stack.head)
    )

  def pop2(l: DataLabel.In): FrameUpdate =
    FrameUpdate(
      newFrame.copy(stack = newFrame.stack.drop(2)),
      binding + (l -> newFrame.stack(1))
    )

  def setLocal(n: Int, data: DataLabel.Out): FrameUpdate =
    FrameUpdate(newFrame.copy(locals = newFrame.locals.updated(n, data)), binding)

  def load1(n: Int): FrameUpdate = push1(newFrame.local(n))

  def store1(n: Int): FrameUpdate = setLocal(n, newFrame.stackTop)

  def push1(d: DataLabel.Out): FrameUpdate =
    FrameUpdate(newFrame.copy(stack = d +: newFrame.stack), binding)

  def push2(d: DataLabel.Out): FrameUpdate =
    FrameUpdate(
      newFrame.copy(stack = DataLabel.out(s"second word of ${d.name}") :: d :: newFrame.stack),
      binding
    )

  def push(t: TypeRef, d: DataLabel.Out): FrameUpdate =
    if(t.isDoubleWord) push2(d) else push1(d)

  // TODO 2word
  def ret(retval: DataLabel.In): FrameUpdate =
    FrameUpdate(
      Frame(Map.empty, List.empty, newFrame.effect),
      binding + (retval -> newFrame.stack.head)
    )
}
object FrameUpdate {
}
