package com.todesking.scalanb

import com.todesking.scalanb.util.TappedPrintStream
import com.todesking.scalanb.util.IO

object Runner {
  def main(args: Array[String]): Unit = {
    val Seq(className) = args.toSeq

    val targetClass = Class.forName(className)
    val run = targetClass.getMethod("scalanb__run", classOf[Builder])

    // TODO: tap stdout/stderr

    val notebookName = targetClass.getSimpleName
    val sdf = new java.text.SimpleDateFormat("yyyyMMdd_HHmmss")
    val logName = s"${sdf.format(new java.util.Date())}_$notebookName"

    val fs = java.nio.file.FileSystems.getDefault
    val basePath = fs.getPath(sys.props("user.home"), ".scalanb", "hist")

    val out = new FileOut(basePath, logName)
    out.prepare()

    val format = Format.Default
    val builder = new Builder.OnMemory(format)

    val nakedOut = System.out

    val tappedOut = TappedPrintStream(System.out) { str =>
      builder.stdout(str)
    }
    val tappedErr = TappedPrintStream(System.out) { str =>
      builder.stderr(str)
    }

    IO.withOuts(tappedOut, tappedErr) {
      try {
        val target = targetClass.newInstance()
        val _ = run.invoke(target, builder)
      } catch {
        case e: Throwable =>
          // TODO: Write incomplete notebook
          throw e
      }
    }

    out.notebook(builder.build())

    nakedOut.println(s"Notebook log saved to ${out.path}")
  }
}
