package ppsec.queue.test

import ppsec.queue.Queue

trait QueueTest extends org.scalatest.FunSpec {
  def newEmptyQueue[A]: Queue[A]

  describe("empty queue") {
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
  }
}

class QueueTest1 extends QueueTest {
  override def newEmptyQueue[A] = Queue.empty[A]
}
