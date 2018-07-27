# scalanb: Scala notebook

Status: Vaporware

## Example

```scala
import com.todesking.{scalanb => nb}

@nb
object NB {
  nb.markdown("""
# Example of scalanb
  """)

  nb.math("""
  a + b = 1
  """)

  val data = Seq(1, 2, 3, 4, 5)
  println("data prepared.")
  data // End block and print result implicitly

  data.map(_ * 2) // End block and print result implicitly

  // Define block explicitly
  nb.block {
    println("Hello!")
    println(s"Sum of data = ${data.sum}")
  }

}
```

and

```
sbt 'runMain NB out.ipynb'
```

Result: (screenshot here)
