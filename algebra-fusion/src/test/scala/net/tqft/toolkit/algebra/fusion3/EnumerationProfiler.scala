package net.tqft.toolkit.algebra.fusion3

object EnumerationProfiler extends App {
  while(true) {
  println(net.tqft.toolkit.Profiler.timing(BruteForceFusionRings.main("5 0 60 -u -q".split(" ")))._1)
  println(net.tqft.toolkit.Profiler.timing(BruteForceFusionRings.main("4 1 60 -u -q".split(" ")))._1)
  println(net.tqft.toolkit.Profiler.timing(BruteForceFusionRings.main("2 2 90 -u -q".split(" ")))._1)
  }
  // 20160303 0930: 35000ms 
}