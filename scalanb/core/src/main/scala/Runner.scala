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

  def newOut(name: String): FileOut = {
    val sdf = new java.text.SimpleDateFormat("yyyyMMdd_HHmmss")
    val logName = s"${sdf.format(new java.util.Date())}_$name"

    val fs = java.nio.file.FileSystems.getDefault
    val basePath = fs.getPath(sys.props("user.home"), ".scalanb", "hist")

    val out = new FileOut(basePath, logName)
    out.prepare()
    out
  }

  type TargetType = {
    def scalanb__run(builder: Builder): Unit
  }

  def runBatch(args: Array[String], target: TargetType, notebookName: String): Unit = {
    import scala.language.reflectiveCalls

    val out = newOut(notebookName)

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
      out.notebook(builder.build())
      println(s"scalanb: Notebook log saved to ${out.path}")
    }
  }
}
