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

  // 一般的な実装(5.2 キュー)
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

  def empty[T]: Queue[T] = new Impl1(Nil, Nil)
}

