package net.tqft.toolkit.algebra.fusion

object FusionBimodulesSandbox extends App {

  val H1 = FusionRings.Examples.H1
  val AH1 = FusionRings.Examples.AH1
  val AH2 = FusionRings.Examples.AH2
  val AH3 = FusionRings.Examples.AH3
  
  for (fm <- AH1.candidateFusionModules) {
    val commutants = FusionBimodules.commutantsWithRank(fm, 15, 3)
    println(fm + " has " + commutants.size + " commutants with rank 15 and 3 self-dual objects.")
  }
}