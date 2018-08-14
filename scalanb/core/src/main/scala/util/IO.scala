package com.todesking.scalanb.util

import java.io.PrintStream

object IO {
  def withOuts[A](stdout: PrintStream, stderr: PrintStream)(f: => A): A = {
    // Ensure to initialize Console
    val _ = Console

    val oldout = System.out
    val olderr = System.err

    System.setOut(stdout)
    System.setErr(stderr)

    val ret =
      Console.withOut(stdout) {
        Console.withErr(stderr) {
          f
        }
      }

    System.setOut(oldout)
    System.setErr(olderr)
    ret
  }

}
