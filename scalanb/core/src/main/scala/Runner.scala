package com.todesking.scalanb

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

    def invoke(): Unit = {
      try {
        val target = targetClass.newInstance()
        val _ = run.invoke(target, builder)
      } catch {
        case e: Throwable =>
          // TODO: Write incomplete notebook
          throw e
      }
    }

    val tappedOut = TappedPrintStream(System.out) { str =>
      builder.stdout(str)
    }
    val tappedErr = TappedPrintStream(System.out) { str =>
      builder.stderr(str)
    }

    // Ensure to initialize Console
    val _ = Console.out

    System.setOut(tappedOut)
    System.setErr(tappedErr)
    Console.withOut(tappedOut) {
      Console.withErr(tappedErr) {
        invoke()
      }
    }

    out.notebook(builder.build())

    nakedOut.println(s"Notebook log saved to ${out.path}")
  }

  // TODO: Support ALL methods
  abstract class TappedPrintStream(original: java.io.PrintStream) extends java.io.PrintStream(original) {
    override def println(s: String) =
      print(s + "\n")
    override def print(s: String) = {
      tap(s)
      original.print(s)
    }
    protected def tap(s: String): Unit
  }
  object TappedPrintStream {
    def apply(o: java.io.PrintStream)(f: String => Unit): TappedPrintStream = new TappedPrintStream(o) {
      override def tap(s: String) = f(s)
    }
  }
}
