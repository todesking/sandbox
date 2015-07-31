object Main {
  def main(args: Array[String]): Unit = {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    { // 3.1
      assert((List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
      }) == 1 + 2)
    }

    { // 3.2
      def tail[A](l: List[A]): List[A] = l match {
        case Cons(h, t) => t
        case Nil => throw new AssertionError()
      }
      assert(tail(List(1, 2, 3)) == List(2, 3))
    }

    { // 3.3
      def setHead[A](l: List[A], newHead: A): List[A] = l match {
        case Cons(h, t) => Cons(newHead, t)
        case Nil => throw new AssertionError()
      }
      assert(setHead(List(1, 2, 3), 10) == List(10, 2, 3))
    }

    { // 3.4
      def drop[A](l: List[A], n: Int): List[A] = l match {
        case _ if n == 0 => l
        case Cons(h, t) => drop(t, n - 1)
        case Nil => throw new AssertionError()
      }
      assert(drop(List(1, 2, 3, 4), 2) == List(3, 4))
    }

    { // 3.5
      def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Cons(h, t) =>
          if(f(h)) dropWhile(t, f)
          else l
        case Nil => Nil
      }
      assert(dropWhile(List(1, 2, 3, 4, 5), (_: Int) < 3) == List(3, 4, 5))
      assert(dropWhile(List[Int](), (_: Int) < 3) == Nil)
    }

    { // 3.6
      def init[A](l: List[A]): List[A] = l match {
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
        case Nil => throw new AssertionError()
      }
      assert(init(List(1, 2, 3)) == List(1, 2))
    }

    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Cons(h, t) => f(h, foldRight(t, z)(f))
      case Nil => z
    }

    { // 3.7
      // there is no short-circuiting if we can't use exceptions or modify foldRight.
      def product(l: List[Double]): Double =
        foldRight(l, 1.0)(_ * _)
      assert(product(List(1.0, 2.0, 3.0)) == 6.0)
    }

    { // 3.8
      assert(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))
    }

    { // 3.9
      def length(l: List[_]): Int = foldRight(l, 0) { (a, len) => len + 1 }
      assert(length(List(1, 2, 3)) == 3)
      assert(length(List()) == 0)
    }

    def foldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Cons(h, t) => foldRight(t, f(h, z))(f)
      case Nil => z
    }
    { // 3.10
      println(foldLeft(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
      assert(foldLeft(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(3, 2, 1))
    }

    { // 3.11
      // omit
    }

    def reverse[A](l: List[A]): List[A] =
      foldLeft(l, Nil: List[A])(Cons(_, _))
    ;{ // 3.12
      assert(reverse(List()) == List())
      assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
    }

    { // 3.13
      def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(foldLeft(l, Nil: List[A])(Cons(_, _)), z)(f)
    }

    { // 3.14
      // omit
    }

    { // 3.15
      def flatten[A](l: List[List[A]]): List[A] =
        ???
    }

    def fib(n: Int): Int = {
      def f(n: Int, n1: Int, n2: Int): Int =
        if(n == 0) n1 + n2
        else f(n - 1, n2, n1 + n2)
      n match {
        case 0 => 0
        case 1 => 1
        case _ => f(n - 2, 0, 1)
      }
    }
      println(fib(0))
      println(fib(1))
      println(fib(2))
      println(fib(3))
      println(fib(4))
  }
    def fib(n: Int): Int = {
      def f(n: Int, n1: Int, n2: Int): Int =
        if(n == 0) n1 + n2
        else f(n - 1, n2, n1 + n2)
      n match {
        case 0 => 0
        case 1 => 1
        case _ => f(n - 2, 0, 1)
      }
    }
}

sealed abstract class List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](values: A*): List[A] =
    if(values.isEmpty) Nil
    else Cons(values.head, apply(values.tail: _*))
}


