package com.todesking.scalanb.ipynb

case class Notebook(
  metadata: Map[String, Notebook.Metadata],
  nbformat: Int,
  nbformatMinor: Int,
  cells: Seq[Cell])

object Notebook {
  class Metadata
  object Metadata {
    case class KernelInfo(name: String) extends Metadata
    case class LanguageInfo(name: String, version: String, codemirrorMode: Option[String]) extends Metadata
  }
}

abstract class Cell(val cellType: String) {
  def metadata: Cell.Metadata = Cell.EmptyMetadata
  def source: String
}
object Cell {
  class Metadata
  case object EmptyMetadata extends Metadata

  case class Markdown(
    source: String,
    attachments: Map[String, Map[String, String]]) extends Cell("markdown")

  case class Code(
    source: String,
    override val metadata: CodeMetadata,
    outputs: Seq[Output]) extends Cell("code") {
  }
  case class CodeMetadata(collapsed: Boolean, autoscroll: Boolean) extends Metadata
}

class Output(val outputType: String) {
}
object Output {
  case class Stream(
    name: String,
    text: String) extends Output("stream")
  case class DisplayData(
    data: Map[String, String],
    metadata: Map[String, String]) extends Output("display_data")
  case class ExecuteResult(
    data: Map[String, String],
    metadata: Map[String, String],
    executionCount: Int) extends Output("execute_result")
  case class Error(
    ename: String,
    evalue: String,
    traceback: Seq[String]) extends Output("error")
}
