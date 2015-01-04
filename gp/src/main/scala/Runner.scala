package com.todesking.scalagp

class Runner[A, C] {
  def run(isle: Isle[A, C], stop: SelectionReport[A, C] => Boolean, describe: Individual[A, C] => String = defaultDescribe): Unit = {
    println("Definitions:")
    isle.repository.allDefinitions.foreach { d =>
      println(s"  * ${d}")
    }

    var report = isle.nextGeneration()
    printReport(isle, report, describe)
    while(!stop(report)) {
      report = isle.nextGeneration()
      printReport(isle, report, describe)
    }
  }

  def defaultDescribe(individual: Individual[A, C]): String = ""

  def printReport(isle: Isle[A, C], report: SelectionReport[A, C], describe: Individual[A, C] => String): Unit = {
    val end = System.nanoTime()
    val (majolity, pop) = report.majolity
    println(s"========== Generation ${isle.generation} ==========")
    println(s"Time: ${report.executionMillis}[ms]")
    println(s"Majolity: population=${pop}, size=${majolity.tree.size}, ${describe(majolity)}")
    println(majolity.tree.toString)
    println(s"Uniqueness: ${report.uniqueIndividuals}/${report.individuals.size}")
    println(s"Size(99, 90, 50): ${report.percentiles(99, 90, 50)(_.tree.size).map(_._1).mkString(", ")}")
    println(s"Depth(99, 90, 50): ${report.percentiles(99, 90, 50)(_.tree.height).map(_._1).mkString(", ")}")
  }
}

