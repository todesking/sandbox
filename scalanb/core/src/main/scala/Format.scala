package com.todesking.scalanb

import scala.reflect.runtime.universe.TypeTag
import com.todesking.scalanb.ipynb.Output
import com.todesking.scalanb.ipynb.Data

trait Format {
  def apply[A: TypeTag](value: A): Output
  def error(t: Throwable): Output
}

object Format {
  object Default extends Format {
    override def apply[A: TypeTag](value: A): Output = {
      Output.ExecuteResult(Data.text(value.toString), Map(), 1)
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
