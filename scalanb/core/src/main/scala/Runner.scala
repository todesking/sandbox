package com.todesking.scalanb

import com.todesking.scalanb.util.TappedPrintStream
import com.todesking.scalanb.util.IO

object Runner {
  def run(format: Format, out: FileOut)(f: Builder => Unit): Unit = {
    val builder = new Builder.OnMemory(format)

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
          // TODO: Write incomplete notebook
          throw e
      }
    }

    out.notebook(builder.build())
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

  def main(args: Array[String]): Unit = {
    val Seq(className) = args.toSeq

    val targetClass = Class.forName(className)
    val runMethod = targetClass.getMethod("scalanb__run", classOf[Builder])

    val notebookName = targetClass.getSimpleName
    val format = Format.Default
    val nakedOut = System.out

    val out = newOut(notebookName)

    run(format, out) { builder =>
      val target = targetClass.newInstance()
      val _ = runMethod.invoke(target, builder)
    }

    nakedOut.println(s"scalanb: Notebook log saved to ${out.path}")
  }
}
