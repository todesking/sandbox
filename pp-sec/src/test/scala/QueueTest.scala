package ppsec.queue.test

import ppsec.queue.Queue

trait QueueTest extends org.scalatest.FunSpec {
  def newEmptyQueue[A]: Queue[A]

  describe("Empty queue") {
    it("should immutable empty queue") {
      val x = newEmptyQueue[Int]
      assert(x.isEmpty)
      assert(x.head == None)

      // check immutability
      x.enQueue(1)
      assert(x.isEmpty)
    }
  }
  describe("Queue") {
    it("should store values in FIFO order") {
      val q = newEmptyQueue[Int].enQueue(1).enQueue(2).enQueue(3)

      assert(q.head == Some(1))
      assert(q.deQueue.head == Some(2))
      assert(q.deQueue.deQueue.head == Some(3))
      assert(q.deQueue.deQueue.deQueue.head == None)
    }
    it("should throw IllegalStateException when dequeing empty queue") {
      assertThrows[IllegalStateException] {
        newEmptyQueue[Int].deQueue
      }
    }
    it("should not break while complex operation") {
      val e = newEmptyQueue[Int]
      val q1 =
        e
          .enQueue(1) // [1]
          .deQueue
          .enQueue(2)
          .enQueue(3) // [2, 3]
          .deQueue
          .enQueue(4)
          .enQueue(5)
          .enQueue(6) // [3, 4, 5, 6]
          .deQueue
      assert(!q1.isEmpty)
      assert(q1.head == Some(4))
      assert(q1.deQueue.head == Some(5))
      assert(q1.deQueue.deQueue.head == Some(6))
      assert(q1.deQueue.deQueue.deQueue.head == None)
    }
  }
}

class QueueTest0 extends QueueTest {
  override def newEmptyQueue[T] = Queue.empty0
}

class QueueTest1 extends QueueTest {
  override def newEmptyQueue[A] = Queue.empty1
}
class QueueTest2 extends QueueTest {
  override def newEmptyQueue[A] = Queue.empty2
}
class QueueTest3 extends QueueTest {
  override def newEmptyQueue[A] = Queue.empty3
}
