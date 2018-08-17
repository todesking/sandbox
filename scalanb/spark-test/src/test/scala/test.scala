@com.todesking.scalanb.spark.Notebook
object SparkNB {
  import spark.implicits._

  val df = spark.createDataset(Seq(1, 2, 3, 4, 5)).toDF("v")
  df
  df.count
  df.select('v + 1)
}
