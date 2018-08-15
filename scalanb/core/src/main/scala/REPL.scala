package com.todesking.scalanb

import scala.tools.nsc.GenericRunnerCommand

object REPL {
  def main(args: Array[String]): Unit = {
    val out = Runner.newOut("REPL")

    val format = Format.Default
    val builder = new Builder.OnMemory(format)

    // TODO: Show value even if tree is ValDef
    val repl = new scala.tools.nsc.interpreter.ScalanbREPL {
      override def hook(interp: Interp)(
        src: String,
        trees: Seq[interp.global.Tree],
        inSilent: Boolean)(eval: () => Either[Throwable, (Any, String)]) = {

        if (!inSilent)
          builder.code(src)

        val result = Runner.run(builder) { b =>
          eval()
        }
        result match {
          case Left(t: java.lang.reflect.InvocationTargetException) =>
            builder.error(t.getCause)
          case Left(t) =>
            builder.error(t)
          case Right((v, s)) =>
            if (!inSilent && s != "")
              builder.expr(v, s)
        }
        result
      }

    }

    val command = new GenericRunnerCommand(List("-usejavacp"), Console.err.println(_))

    val _ = repl.process(command.settings)

    out.notebook(builder.build())
    println(s"scalanb: Notebook log saved to ${out.path}")
  }
}
