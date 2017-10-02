package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.Profiler

/**
 * @author scott
 */
object BoundaryConnectedPlanarGraphsProfiling extends App {
  for (k <- 0 until 20) {
    println(Profiler.timing(BoundaryConnectedPlanarGraphs.trivalent(4, k).size))
  }
}