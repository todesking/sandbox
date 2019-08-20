package ppsec.queue

trait Queue[T] {
  def isEmpty: Boolean
  def enQueue(t: T): Queue[T]
  // Removes the element at the beginning of the immutable queue, and returns the new queue.
  def deQueue(): Queue[T]
  def head: Option[T]
}
object Queue {
  // ref: Chris Okasaki(著), 稲葉一浩, 遠藤侑介(訳) (2017). 粋関数型データ構造 株式会社ドワンゴ

  // Simple immutable Queue (Queue in 5.2)
  // enQueue, head, isEmpty is O(1)
  // deQueue is O(1)(amortized), O(N)(worst-case)
  // If this queue is reused(e.g. `val x = queue.deQueue; val y = queue.deQueue`),
  // amortized cost could O(N)
  class Impl1[T](private[this] val f: List[T], private[this] val r: List[T]) extends Queue[T] {
    override def isEmpty = f.isEmpty

    override def enQueue(t: T) =
      if (isEmpty) new Impl1(t :: Nil, Nil)
      else new Impl1(f, t :: r)

    override def deQueue() = (f, r) match {
      case (x :: Nil, r) => new Impl1(r.reverse, Nil)
      case (x :: f, r) => new Impl1(f, r)
      case _ => throw new IllegalStateException("Queue is empty")
    }

    override def head = f.headOption
  }
  def empty1[T]: Impl1[T] = new Impl1(Nil, Nil)

  // Immutable Queue with delayed stream (BankersQueue in 6.3.2)
  // head, isEmpty is O(1)
  // enQueue and deQueue is O(1)(amortized), O(N)(worst-case)
  class Impl2[T](
    private[this] val lenf: Int,
    private[this] val f: LazyList[T],
    private[this] val lenr: Int,
    private[this] val r: LazyList[T]) extends Queue[T] {
    override def isEmpty = lenf == 0

    override def enQueue(t: T) = check(lenf, f, lenr + 1, t #:: r)

    override def deQueue =
      if (isEmpty) throw new IllegalStateException("Queue is empty")
      else check(lenf - 1, f.tail, lenr, r)

    override def head =
      if (isEmpty) None
      else Some(f.head)

    private[this] def check(lenf2: Int, f2: LazyList[T], lenr2: Int, r2: LazyList[T]) =
      if (lenr2 <= lenf2) new Impl2(lenf2, f2, lenr2, r2)
      else new Impl2(lenf2 + lenr2, f2 ++ r2.reverse, 0, LazyList.empty)
  }
  def empty2[T]: Impl2[T] = new Impl2[T](0, LazyList.empty, 0, LazyList.empty)

  // Immutable queue that every operation is O(1) (even in worst-case)
  // (Real-time queue in 7.2)
  class Impl3[T](
    private[this] val f: LazyList[T],
    private[this] val r: List[T],
    private[this] val s: LazyList[T]) extends Queue[T] {
    override def isEmpty = f.isEmpty

    private[this] def rotate(f2: LazyList[T], r2: List[T], s2: LazyList[T]): LazyList[T] =
      if (f2.isEmpty) r2.head #:: s2
      else f2.head #:: rotate(f2.tail, r2.tail, r2.head #:: s2)

    private[this] def exec(f2: LazyList[T], r2: List[T], s2: LazyList[T]) =
      if (s2.isEmpty) {
        val ff = rotate(f2, r2, s2)
        new Impl3(ff, Nil, ff)
      } else {
        new Impl3(f2, r2, s2.tail)
      }

    override def enQueue(t: T) = exec(f, t :: r, s)

    override def deQueue =
      if (isEmpty) throw new IllegalStateException("Queue is empty")
      else exec(f.tail, r, s)

    override def head = f.headOption
  }
  def empty3[T]: Impl3[T] = new Impl3(LazyList.empty, Nil, LazyList.empty)

  def empty[T]: Queue[T] = empty1
}

