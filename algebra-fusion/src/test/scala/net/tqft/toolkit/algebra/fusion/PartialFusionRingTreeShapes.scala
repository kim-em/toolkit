package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.Profiler
import net.tqft.toolkit.collections.FlexibleTranspose._

object PartialFusionRingTreeShapes extends App {

  val enumeration = PartialFusionRingEnumeration(5, 0)

  for (steps <- 0 until 10) {
    println(Profiler.timing(enumeration.root.descendants({ r => (1 - r.level) * (steps - r.entries.size) }).size))
  }
}