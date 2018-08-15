package com.todesking.scalanb

import com.todesking.scalanb.ipynb.Output
import com.todesking.scalanb.ipynb.Data

trait Format {
  def apply(value: Any): Output
  def apply(value: Any, str: String): Output
  def error(t: Throwable): Output
}

object Format {
  object Default extends Format {
    override def apply(value: Any) =
      apply(value, s"$value")
    override def apply(value: Any, str: String): Output = {
      Output.ExecuteResult(Data.text(str), Map(), 1)
    }
    override def error(t: Throwable) = {
      val stackTraceMessage = t.getStackTrace.map { st =>
        s"  ${st.toString}"
      }
      Output.Error(
        "Exception",
        "(Does Jupyter really use this field??)",
        Seq(t.toString) ++ stackTraceMessage)
    }
  }
}
