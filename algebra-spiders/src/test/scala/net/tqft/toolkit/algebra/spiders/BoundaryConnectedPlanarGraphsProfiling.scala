package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.Profiler

/**
 * @author scott
 */
object BoundaryConnectedPlanarGraphsProfiling extends App {
  for (k <- 0 until 10) {
    println(Profiler.timing(BoundaryConnectedPlanarGraphs.trivalent(8, k).size))
  }
}