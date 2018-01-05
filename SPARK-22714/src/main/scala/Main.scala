package com.todesking.SPARK_22714

import org.apache.spark.sql.SparkSession

object Main {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder().getOrCreate()
    import spark.implicits._

    val a = new Array[Int](4 * 1000 * 1000)
    val ds = spark.createDataset(a)
    ds.rdd.zipWithIndex
  }
}
