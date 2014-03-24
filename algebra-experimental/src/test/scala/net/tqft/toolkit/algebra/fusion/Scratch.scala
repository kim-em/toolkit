package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.grouptheory._

object Scratch extends App {

  for(g <- List(/*FiniteGroups.Mathieu11,*/ FiniteGroups.Mathieu12 /*, FiniteGroups.Mathieu22, FiniteGroups.Mathieu23, FiniteGroups.Mathieu24*/)) {
    println(g.generators)
    val ring = FusionRings.Examples.representationsOf(g)
    for(i <- 0 until ring.rank) {
      println(ring.multiply(ring.basis(i), ring.basis(ring.duality(i))))
    }
  }
}