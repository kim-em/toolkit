package net.tqft.toolkit.algebra.fusion3

object Profiler extends App {
  println(net.tqft.toolkit.Profiler.timing(BruteForceFusionRings.main("5 0 60".split(" ")))._1)
//  println(net.tqft.toolkit.Profiler.timing(BruteForceFusionRings.main("6 0 30".split(" ")))._1)
}