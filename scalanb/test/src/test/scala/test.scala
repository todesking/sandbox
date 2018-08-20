import com.todesking.{ scalanb => nb }

@nb.Notebook
class NBTest {
  println("hello")

  val abc = 1
  println(s"abc = $abc")
  abc

  System.out.println("Hello from System.out.println")
  println("Hello from Console.println")
  System.err.println("Boo!")
  Console.err.println("Boo!2")
  val b = abc + 1
  val c = abc * b
  c
  b
  println(1)

  def f(xxx: Int) = {
    xxx * xxx
  }

  f(10); f(20)

  val xxx = 100
  val yyy = 200
  Thread.sleep(6000)
  val zzz = 300

  ???
}
