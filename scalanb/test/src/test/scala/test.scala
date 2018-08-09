import com.todesking.{ scalanb => nb }

@nb.Notebook
class NBTest {
  val a = 1
  println(s"a = $a")
  a

  val b = a + 1
  val c = a * b
  c
  b
  println(1)
}
