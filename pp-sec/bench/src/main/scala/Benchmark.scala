package ppsec.queue.benchmark

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole

import ppsec.queue.Queue

trait QueueBenchmark {
  def newEmptyQueue[T]: Queue[T]

  @Benchmark
  def enqueue_then_dequeue(bh: Blackhole) = {
    var q = newEmptyQueue[Int]
    var i = 0
    while (i < 1000) {
      q = q.enQueue(i)
      i += 1
    }
    while (!q.isEmpty) {
      bh.consume(q.head)
      q = q.deQueue
    }
    q
  }

  @Benchmark
  def reuse(bh: Blackhole) = {
    var q = newEmptyQueue[Int]
    var i = 0
    while (i < 1000) {
      q = q.enQueue(i)
      i += 1
    }
    i = 0
    while (i < 1000) {
      bh.consume(q.head)
      q.deQueue
      i += 1
    }
    q
  }

  @Benchmark
  def enqueue_and_dequeue(bh: Blackhole) = {
    var q = newEmptyQueue[Int]
    var i = 0
    // E: 200
    while (i < 200) {
      q = q.enQueue(i)
      i += 1
    }

    // D: 100
    i = 0
    while (i < 100) {
      bh.consume(q.head)
      q = q.deQueue
      i += 1
    }

    // E: +400(600), D: +200(300)
    i = 0
    while (i < 200) {
      q = q.enQueue(i)
      q = q.enQueue(i)
      bh.consume(q.head)
      q = q.deQueue
      i += 1
    }
    // E: +200(800), D: +400(700)
    i = 0
    while (i < 200) {
      bh.consume(q.head)
      q = q.deQueue
      bh.consume(q.head)
      q = q.deQueue
      q = q.enQueue(i)
      i += 1
    }
    // E: +200(1000), D: +200(900)
    i = 0
    while (i < 200) {
      q = q.enQueue(i)
      bh.consume(q.head)
      q = q.deQueue
      i += 1
    }

    // D: +100(1000)
    i = 0
    while (i < 100) {
      bh.consume(q.head)
      q = q.deQueue
      i += 1
    }
    q
  }
}

class QueueBenchmark0 extends QueueBenchmark {
  override def newEmptyQueue[T] = Queue.empty0[T]
}
class QueueBenchmark1 extends QueueBenchmark {
  override def newEmptyQueue[T] = Queue.empty1[T]
}
class QueueBenchmark2 extends QueueBenchmark {
  override def newEmptyQueue[T] = Queue.empty2[T]
}
class QueueBenchmark3 extends QueueBenchmark {
  override def newEmptyQueue[T] = Queue.empty3[T]
}
