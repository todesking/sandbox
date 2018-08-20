package com.todesking.scalanb

import com.todesking.scalanb.ipynb.Cell
import com.todesking.scalanb.ipynb.Output

trait Builder {
  protected def executionCount: Int

  protected def flush(res: Option[Output]): Unit

  def code(src: String): Unit

  def expr(value: Unit): Unit = {}
  def expr(value: Nothing): Unit = {}

  def expr(value: Value): Unit =
    flush(Some(Output.ExecuteResult(value.data, Map(), executionCount)))

  def expr[A: Format](value: A): Unit =
    expr(implicitly[Format[A]].apply(value))

  def markdownCell(src: String): Unit

  def error(t: Throwable)(implicit format: ErrorFormat): Unit =
    flush(Some(format.apply(t)))

  def stdout(s: String): Unit
  def stderr(s: String): Unit

  def build(): ipynb.Notebook
}

object Builder {
  case class ExecLog(
    code: String,
    startAt: Long,
    duration: Long,
    stdout: Seq[String],
    stderr: Seq[String]) {
    def addStdout(s: String) = copy(stdout = this.stdout :+ s)
    def addStderr(s: String) = copy(stderr = this.stderr :+ s)
    def setDuration(t: Long) = copy(duration = t)
  }
  object ExecLog {
    def apply(code: String, startAt: Long): ExecLog = ExecLog(code, startAt, startAt, Seq(), Seq())
  }

  class OnMemory extends Builder {
    private[this] var _executionCount = 1
    override def executionCount = _executionCount

    private[this] var cells = Seq.empty[Cell]

    private[this] var execLogs = Seq.empty[ExecLog]
    private[this] var currentExecLog: Option[ExecLog] = None

    val showTimeMillis = 5 * 1000

    private[this] def addCell(c: Cell) = {
      this.cells = this.cells :+ c
    }

    override def code(s: String) = {
      currentExecLog.foreach { el =>
        val duration = System.currentTimeMillis() - el.startAt
        if (duration > showTimeMillis) {
          this.currentExecLog = None
          flush(None)
          this.currentExecLog = Some(el)
          val out = ipynb.Output.DisplayData(Value.text(f"Execution time: ${duration / 1000.0}%.2f").data, Map())
          flush(Some(out))
        } else {
          this.execLogs = this.execLogs :+ el.setDuration(duration)
        }
      }
      this.currentExecLog = Some(ExecLog(s, System.currentTimeMillis()))
    }

    override def stdout(s: String) = {
      this.currentExecLog = currentExecLog.map(_.addStdout(s))
    }

    override def stderr(s: String) = {
      this.currentExecLog = currentExecLog.map(_.addStderr(s))
    }

    override def markdownCell(s: String) = {
      flush(None)
      addCell(Cell.Markdown(s))
    }

    private[this] def flushCell(els: Seq[ExecLog], res: Option[Output]): Unit = {
      var outputs = Seq.empty[Output]
      els.foreach { el =>
        if (el.stdout.nonEmpty) {
          outputs = outputs :+ Output.Stream(
            "stdout",
            el.stdout.mkString("") + "\n")
        }
        if (el.stderr.nonEmpty) {
          outputs = outputs :+ Output.Stream(
            "stderr",
            el.stderr.mkString("") + "\n")
        }
      }
      res.foreach { r =>
        outputs = outputs :+ r
      }
      if (els.nonEmpty) {
        addCell(Cell.Code(
          executionCount = executionCount,
          source = els.map(_.code).mkString("\n"),
          metadata = Cell.CodeMetadata(
            collapsed = false, autoscroll = false),
          outputs = outputs))
        this._executionCount += 1
      }
    }

    override def flush(res: Option[Output]) = {
      flushCell(execLogs, None)
      flushCell(currentExecLog.toSeq, res)
      this.currentExecLog = None
      this.execLogs = Seq()
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
