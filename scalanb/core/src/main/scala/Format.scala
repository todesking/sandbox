package com.todesking.scalanb

import com.todesking.scalanb.ipynb.Output

trait Format[-A] {
  def apply(value: A): Value
}

trait ErrorFormat {
  def apply(t: Throwable): Output.Error
}

trait FormatLowPriorityImplicit {
  implicit val defaultAny: Format[Any] = new Format[Any] {
    override def apply(value: Any) = Value.text(s"$value")
  }
}
object Format extends FormatLowPriorityImplicit {
}

object ErrorFormat {
  implicit val default: ErrorFormat = new ErrorFormat {
    override def apply(t: Throwable) = {
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
