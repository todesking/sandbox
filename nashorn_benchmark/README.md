# Benchmark: Neive interpreter, Rhino, and Nashorn

## JDK 1.8.0

```
$ sbt -J-server run
...
Neive(N=12000000): 4656[ms]
Oracle Nashorn(N=12000000): 1873[ms]
Oracle Nashorn define function(cached)(N=1000): 103[ms]
Oracle Nashorn define function(uncached)(N=1000): 2257[ms]
```
