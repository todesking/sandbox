# scalanb: Scala notebook

Status: PoC

## Installation

```sbt
// In build.sbt

// REPL and batch
libraryDependencies += "com.todesking" %% "scalanb" % "0.0.1"

// Spark REPL and Spark batch
libraryDependencies += "com.todesking" %% "scalanb-spark" % "0.0.1"

// To use batch notebook, you need macro paradise plugin and additional compiler options.
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
```

## Example: Batch Notebook

```scala
import com.todesking.scalanb

@scalanb.Notebook
class MyNotebook {
  nb.markdown("""
# Example of scalanb
  """)

  val data = Seq(1, 2, 3, 4, 5)
  println("data prepared.")
  data // End block and print result implicitly

  data.map(_ * 2) // End block and print result implicitly

  println("Hello!")
  println(s"Sum of data = ${data.sum}")
}
```

and

```shellsession
$ sbt 'runMain MyNotebook'
```

Result: (screenshot here)

In default, notebooks (`.ipynb`) are saved to `~/.scalanb/hist/`


To specify history location, use `--out` option.

```shellsession
$ sbt 'runMain MyNotebook --out=file:path=./hist/'
```

## Example: REPL Notebook

```shellsession
$ sbt 'runMain com.todesking.scalanb.REPL'

scala> // ...
scala> :q

$ ls ~/.scalanb/hist/
20180811.031230.repl.ipynb
20180811.031230.repl.scala
```

## Example: Save history to HDFS

```shellsession
$ sbt 'runMain MyNotebook --out=hdfs:path=/tmp/hist/'
```

## Example: Spark Batch Notebook

```scala
import com.todesking.scalanb

@scalanb.spark.Notebook
class MyNotebook {
  // spark session available
  val df = spark.read.csv("...")

  // Show dataframe as HTML tables via `shownb` method
  df.shownb(10)
}
```

```shellsession
$ sbt assembly # Make fatjar
$ spark-submit --class MyNotebook myapp.jar
```

## Example: Spark REPL Notebook

```shellsession
$ sbt assembly # Make fatjar
$ spark-submit --class com.todesking.scalanb.spark.REPL --deploy-mode client myapp.jar
```
