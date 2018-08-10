package com.todesking.scalanb

import scala.tools.nsc.GenericRunnerCommand

object REPL {
  def main(args: Array[String]): Unit = {
    println(Seq.getClass)
    println(Seq.getClass.getClassLoader)
    val repl = new scala.tools.nsc.interpreter.ScalanbREPL
    val command = new GenericRunnerCommand(List("-usejavacp"), Console.err.println(_))
    println(command.ok)
    println(command.settings.usejavacp)
    command.settings.classpathURLs.foreach(println)
    val _ = repl.process(command.settings)
  }
}
