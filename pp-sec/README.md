# Various implementations of immutable queue

0. Wrapper of `scala.collection.immutable.Queue`
1. Basic immutable queue(double list) [PFDS]
2. Banker's queue [PFDS]
3. Real-time queue [PFDS]

reference: [PFDS] Chris Okasaki(著), 稲葉一浩, 遠藤侑介(訳) (2017). 純粋関数型データ構造 株式会社ドワンゴ

* [Implementation](src/main/scala/Queue.scala)
* [Test](src/test/scala/QueueTest.scala)
* [Benchmark](bench/src/main/scala/Benchmark.scala)

## Benchmark

```
sbt 'bench/jmh:run -i 10 -wi 5 -f 1  -t 3 -o jmh.txt'
```
See [sbt-jmh docs](https://github.com/ktoso/sbt-jmh#options) for more information.

### Result

```
Benchmark                              Mode  Cnt      Score      Error  Units
QueueBenchmark0.enqueue_and_dequeue   thrpt   10  66965.479 ± 7168.762  ops/s
QueueBenchmark0.enqueue_then_dequeue  thrpt   10  55974.310 ± 6913.796  ops/s
QueueBenchmark0.reuse                 thrpt   10    305.680 ±   30.161  ops/s
QueueBenchmark1.enqueue_and_dequeue   thrpt   10  97448.420 ± 2461.781  ops/s
QueueBenchmark1.enqueue_then_dequeue  thrpt   10  95638.845 ± 2104.797  ops/s
QueueBenchmark1.reuse                 thrpt   10    511.732 ±    4.416  ops/s
QueueBenchmark2.enqueue_and_dequeue   thrpt   10  18822.609   2282.592  ops/s
QueueBenchmark2.enqueue_then_dequeue  thrpt   10  19171.304    245.088  ops/s
QueueBenchmark2.reuse                 thrpt   10  45116.799 ± 1439.684  ops/s
QueueBenchmark3.enqueue_and_dequeue   thrpt   10  20259.255 ± 1692.642  ops/s
QueueBenchmark3.enqueue_then_dequeue  thrpt   10  21045.437 ± 1871.097  ops/s
QueueBenchmark3.reuse                 thrpt   10  35835.671 ± 1079.263  ops/s
```

To compare Queue2 and Queue3, more experiments(e.g. Plot processing time distribution of enQueue and deQueue) is required to confirm Queue3's real-timeness.
