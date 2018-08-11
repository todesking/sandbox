import com.todesking.{ scalanb => nb }

@nb.Notebook
class NBTest {
  println("hello")

  val abc = 1
  println(s"abc = $abc")
  abc

  val b = abc + 1
  val c = abc * b
  c
  b
  println(1)

  def f(xxx: Int) = {
    xxx * xxx
  }

  f(10); f(20)
}
