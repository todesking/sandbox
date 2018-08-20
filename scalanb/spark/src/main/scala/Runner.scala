package com.todesking.scalanb.spark

import com.todesking.scalanb
import scalanb.Builder
import scalanb.Format

import org.apache.spark.sql.SparkSession

object Runner {
  type TargetType = {
    def scalanb__run(builder: Builder, spark: SparkSession): Unit
  }

  def runBatch(args: Array[String], target: TargetType, notebookName: String): Unit = {
    import scala.language.reflectiveCalls

    val logName = scalanb.Runner.newLogName(notebookName)
    val (parsedArgs, _) = scalanb.Runner.parseArgs(args)

    val spark = SparkSession.builder()
      .appName(s"Notebook:$notebookName").getOrCreate()

    val out = parsedArgs.out

    val builder = new Builder.OnMemory()

    try {
      scalanb.Runner.run(builder) { builder =>
        val _ = try {
          target.scalanb__run(builder, spark)
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
