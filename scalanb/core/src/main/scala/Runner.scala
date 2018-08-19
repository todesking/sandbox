package com.todesking.scalanb

import com.todesking.scalanb.util.TappedPrintStream
import com.todesking.scalanb.util.IO

object Runner {
  def run[A](builder: Builder)(f: Builder => A): A = {
    val tappedOut = TappedPrintStream(System.out) { str =>
      builder.stdout(str)
    }
    val tappedErr = TappedPrintStream(System.out) { str =>
      builder.stderr(str)
    }

    IO.withOuts(tappedOut, tappedErr) {
      try {
        f(builder)
      } catch {
        case e: Throwable =>
          builder.error(e)
          // TODO: Write incomplete notebook
          throw e
      }
    }
  }

  def newLogName(name: String): String = {
    val sdf = new java.text.SimpleDateFormat("yyyyMMdd_HHmmss")
    s"${sdf.format(new java.util.Date())}_$name"
  }

  def newOut(outType: String, args: Map[String, String]): Out = {
    import scala.collection.JavaConverters._
    val loader = java.util.ServiceLoader.load(classOf[OutFactory])
    loader.iterator.asScala
      .toSeq
      .filter(_.name == outType)
      .headOption
      .getOrElse { throw new RuntimeException(s"Unknown out type: $outType") }
      .newOut(args)
  }

  type TargetType = {
    def scalanb__run(builder: Builder): Unit
  }

  def runBatch(args: Array[String], target: TargetType, notebookName: String): Unit = {
    import scala.language.reflectiveCalls

    val logName = newLogName(notebookName)
    val out = newOut("file", Map())

    val builder = new Builder.OnMemory()

    try {
      run(builder) { builder =>
        val _ = try {
          target.scalanb__run(builder)
        } catch {
          case e: java.lang.reflect.InvocationTargetException =>
            throw e.getCause
        }
      }
    } finally {
      val filePath = out.notebook(logName, builder.build())
      println(s"scalanb: Notebook log saved to ${filePath}")
    }
  }
}
