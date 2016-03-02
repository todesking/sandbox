package com.todesking.hoge

case class Frame(locals: Map[Int, (DataLabel.Out, Data)], stack: List[(DataLabel.Out, Data)], effect: Effect) {
  def local(n: Int): (DataLabel.Out, Data) =
    locals(n)

  def stackTop: (DataLabel.Out, Data) = stack.head
}

