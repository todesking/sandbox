package com.todesking.scalanb

import scala.reflect.runtime.universe.TypeTag
import com.todesking.scalanb.ipynb.Output

trait Format {
  def apply[A: TypeTag](value: A): Output
  def error(t: Throwable): Output
}

object Format {
  object Default extends Format {
    override def apply[A: TypeTag](value: A): Output = {
      Output.ExecuteResult(Map("text/plain" -> value.toString), Map(), 1)
    }
    override def error(t: Throwable) = {
      Output.Error(s"Exception: ${t.getClass.getName}", t.getMessage, t.getStackTrace.map(_.toString))
    }
  }
}
