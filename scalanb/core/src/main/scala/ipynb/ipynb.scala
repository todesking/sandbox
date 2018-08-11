package com.todesking.scalanb.ipynb

import play.api.libs.json.JsValue

object JsonMapping {
  import scala.language.implicitConversions
  import play.api.libs.json._

  private[this] def jso(kvs: (Symbol, JsValue)*): JsObject =
    JsObject(kvs.map { case (k, v) => (k.name -> v) })

  private[this] implicit def autoconv[A: Writes](v: A): JsValue = implicitly[Writes[A]].writes(v)

  private[this] implicit def wSeq[A: Writes] = Writes[Seq[A]] { s =>
    val w = implicitly[Writes[A]]
    JsArray(s.map(w.writes))
  }
  implicit lazy val wNotebook: Writes[Notebook] = Writes[Notebook] {
    case Notebook(metadata, nbformat, nbformatMinor, cells) =>
      jso('metadata -> metadata, 'nbformat -> nbformat, 'nbformat_minor -> nbformatMinor, 'cells -> cells)
  }
  implicit lazy val wCell: Writes[Cell] = Writes[Cell] {
    case c @ Cell.Markdown(source) =>
      jso('cell_type -> c.cellType, 'source -> source)
    case c @ Cell.Code(executionCount, source, Cell.CodeMetadata(collapsed, scroll), outputs) =>
      jso(
        'cell_type -> c.cellType,
        'execution_count -> executionCount,
        'source -> source,
        'metadata -> jso(
          'collapsed -> collapsed,
          'scroll -> scroll),
        'outputs -> outputs)
  }
  implicit lazy val wOutput: Writes[Output] = Writes[Output] {
    case v @ Output.Stream(name, text) =>
      jso('output_type -> v.outputType, 'name -> name, 'text -> text)
    case v @ Output.DisplayData(data, metadata) =>
      jso('output_type -> v.outputType, 'data -> data, 'metadata -> metadata)
    case v @ Output.ExecuteResult(data, metadata, executionCount) =>
      jso('output_type -> v.outputType, 'data -> data, 'metadata -> metadata, 'execution_count -> executionCount)
    case v @ Output.Error(ename, evalue, traceback) =>
      jso('output_type -> v.outputType, 'ename -> ename, 'evalue -> evalue, 'traceback -> traceback)
  }

  def toJson(v: Notebook, pretty: Boolean): String =
    toJson[Notebook](v, pretty)
  def toJson[A: Writes](v: A, pretty: Boolean): String =
    if (pretty) Json.prettyPrint(v)
    else Json.stringify(v)
}

object Data {
  import play.api.libs.json._

  def text(v: String): Map[String, JsValue] =
    Map("text/plain" -> JsString(v))

  def html(v: String): Map[String, JsValue] =
    Map("text/html" -> JsString(v))
}

case class Notebook(
  metadata: Map[String, JsValue],
  nbformat: Int,
  nbformatMinor: Int,
  cells: Seq[Cell])

sealed abstract class Cell(val cellType: String) {
  def source: String
}
object Cell {
  case class Markdown(source: String) extends Cell("markdown")

  case class Code(
    executionCount: Int,
    source: String,
    metadata: CodeMetadata,
    outputs: Seq[Output]) extends Cell("code") {
  }

  case class CodeMetadata(collapsed: Boolean, autoscroll: Boolean)
}

class Output(val outputType: String) {
}
object Output {
  case class Stream(
    name: String,
    text: String) extends Output("stream")
  case class DisplayData(
    data: Map[String, JsValue],
    metadata: Map[String, String]) extends Output("display_data")
  case class ExecuteResult(
    data: Map[String, JsValue],
    metadata: Map[String, JsValue],
    executionCount: Int) extends Output("execute_result")
  case class Error(
    ename: String,
    evalue: String,
    traceback: Seq[String]) extends Output("error")
}
