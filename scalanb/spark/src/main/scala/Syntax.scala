package com.todesking.scalanb.spark

import org.apache.spark.sql.Dataset

import com.todesking.scalanb.Value

object Syntax {
  implicit class DatasetNB[A](val self: Dataset[A]) {
    def nb: NBOps[A] = new NBOps[A](self)
  }

  class NBOps[A](self: Dataset[A]) {
    def show(n: Int = 10): Value = {
      val df = self.toDF()
      val cols = df.columns
      val rows = df.limit(n).collect().map { row =>
        (0 until cols.size).map { i =>
          val value = row.get(i)
          // TODO: format value
          s"$value"
        }
      }.toSeq
      Util.table(cols.toSeq, rows)
    }
  }
}

object Util {
  private[this] def strWidth(s: String) = s.toCharArray.map {
    case c if c < 256 => 1
    case _ => 2
  }.sum

  def table(colNames: Seq[String], rows: Seq[Seq[String]]): Value = {
    require(rows.forall { r => r.size == colNames.size })

    def lpad(str: String, w: Int): String = {
      val sw = strWidth(str)
      if (sw < w) " " * (w - sw) + str
      else str
    }

    def renderText(): Value = {
      val colSize = colNames.size
      val widths = (0 until colSize).map { i =>
        (strWidth(colNames(i)) +: rows.map(r => strWidth(r(i)))).max
      }
      val header = colNames.zip(widths).map { case (s, w) => lpad(s, w) }.mkString(", ")
      val body = rows.map { row =>
        row.zip(widths).map { case (s, w) => lpad(s, w) }.mkString(", ")
      }
      Value.text((header +: body).mkString("\n"))
    }

    def h(s: String): String = s
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll("\"", "&quot;")
      .replaceAll("'", "&#39;")

    def renderHtml(): Value = Value.html(
      s"""<table>
      <thead>${
        colNames.map { s => s"<th>${h(s)}</th>" }.mkString("")
      }</thead>
      <tbody>${
        rows.map { row =>
          s"<tr>${row.map { s => s"<td>${h(s)}</td>" }.mkString("")}</tr>"
        }.mkString("\n")
      }</tbody>
      </table>""")

    renderText() ++ renderHtml()
  }
}
