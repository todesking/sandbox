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

  case class Args(out: Out)

  def parseArgs(args: Seq[String]): (Args, Seq[String]) = {
    val rest = {
      val a = args.dropWhile(_ != "--")
      if (a.isEmpty) a else a.tail
    }
    val opts = args.takeWhile(_ != "--")

    var outs = Seq.empty[Out]
    val id = "[a-zA-Z0-9_.]+"
    val outOptionPattern = s"--out=($id)(?::(.+))?".r
    opts.foreach {
      case `outOptionPattern`(outType, outArgs) =>
        val parsedOutArgs = outArgs.split(",").map { kv => kv.split("=") match { case Array(k, v) => (k, v) } }.toMap
        outs = outs :+ newOut(outType, parsedOutArgs)
    }
    val theOut = outs match {
      case Seq() => newOut("file", Map())
      case Seq(o) => o
      case xs => new MultiOut(xs)
    }
    (Args(theOut), rest)
  }

  def runBatch(args: Array[String], target: TargetType, notebookName: String): Unit = {
    import scala.language.reflectiveCalls

    val (parsedArgs, _) = parseArgs(args)

    val logName = newLogName(notebookName)
    val out = parsedArgs.out

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
