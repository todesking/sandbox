package com.todesking.scalanb

import com.todesking.scalanb.ipynb.Cell
import com.todesking.scalanb.ipynb.Output

import scala.reflect.runtime.universe.TypeTag

trait Builder {
  val format: Format

  // res: result or error
  protected def flush(res: Option[Output]): Unit

  def code(src: String): Unit

  def expr(value: Unit): Unit = {}

  def expr[A: TypeTag](value: A): Unit =
    flush(Some(format(value)))

  def markdownCell(src: String): Unit

  def error(t: Throwable): Unit =
    flush(Some(format.error(t)))

  def stdout(s: String): Unit
  def stderr(s: String): Unit

  def build(): ipynb.Notebook
}

object Builder {
  class OnMemory(override val format: Format) extends Builder {

    private[this] var cells = Seq.empty[Cell]
    private[this] var currentSrc = Seq.empty[String]
    private[this] var currentStdout = Seq.empty[String]
    private[this] var currentStderr = Seq.empty[String]

    private[this] def addCell(c: Cell) = {
      this.cells = this.cells :+ c
    }

    override def code(s: String) = {
      this.currentSrc = this.currentSrc :+ s
    }

    override def stdout(s: String) = {
      this.currentStdout = this.currentStdout :+ s
    }

    override def stderr(s: String) = {
      this.currentStderr = this.currentStderr :+ s
    }

    override def markdownCell(s: String) =
      addCell(Cell.Markdown(s))

    override def flush(res: Option[Output]) =
      if (currentSrc.nonEmpty) {
        var outputs = Seq.empty[Output]
        if (currentStdout.nonEmpty) {
          outputs = outputs :+ Output.Stream(
            "stdout",
            currentStdout.mkString("\n"))
          currentStdout = Seq()
        }
        if (currentStderr.nonEmpty) {
          outputs = outputs :+ Output.Stream(
            "stderr",
            currentStderr.mkString("\n"))
          currentStderr = Seq()
        }
        res.foreach { r =>
          outputs = outputs :+ r
        }

        val source = currentSrc.mkString("\n")
        this.currentSrc = Seq()

        addCell(Cell.Code(
          executionCount = 1,
          source = source,
          metadata = Cell.CodeMetadata(
            collapsed = false, autoscroll = false),
          outputs = outputs))
      }

    override def build() = {
      flush(None)
      ipynb.Notebook(
        metadata = Map(),
        nbformat = 4,
        nbformatMinor = 0,
        cells)
    }
  }

}
