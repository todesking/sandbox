package com.todesking.scalanb.util

// TODO: Support ALL methods
abstract class TappedPrintStream(original: java.io.PrintStream) extends java.io.PrintStream(original) {
  override def println(s: String) =
    print(s + "\n")
  override def print(s: String) = {
    tap(s)
    original.print(s)
  }
  protected def tap(s: String): Unit
}
object TappedPrintStream {
  def apply(o: java.io.PrintStream)(f: String => Unit): TappedPrintStream = new TappedPrintStream(o) {
    override def tap(s: String) = f(s)
  }
}
