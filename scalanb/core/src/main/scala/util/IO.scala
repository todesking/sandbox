package com.todesking.scalanb.util

import java.io.PrintStream

object IO {
  def withOuts[A](stdout: PrintStream, stderr: PrintStream)(f: => A): A = {
    // Ensure to initialize Console
    val _ = Console

    System.setOut(stdout)
    System.setErr(stderr)
    Console.withOut(stdout) {
      Console.withErr(stderr) {
        f
      }
    }
  }

}
