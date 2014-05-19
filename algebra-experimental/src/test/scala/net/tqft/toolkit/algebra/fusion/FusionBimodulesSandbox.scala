package net.tqft.toolkit.algebra.fusion

object FusionBimodulesSandbox extends App {

  val H1 = FusionRings.Examples.H1
  val AH1 = FusionRings.Examples.AH1
  val AH2 = FusionRings.Examples.AH2
  val AH3 = FusionRings.Examples.AH3

  val `2D2` = FusionRings.Examples.`2D2`

//  for (fm <- H1.candidateFusionModules) {
//    println("Found a fusion module with algebra object " + fm.algebraObject)
//    println(fm.structureCoefficients)
//  }

  for (fm <- `2D2`.candidateFusionModules) {
    println("Found a fusion module with algebra object " + fm.algebraObject)
    println(fm.structureCoefficients)
  }

  //  for (fm <- AH1.candidateFusionModules) {
  //    val commutants = FusionBimodules.commutantsWithRank(fm, 15, 3)
  //    println(fm + " has " + commutants.size + " commutants with rank 15 and 3 self-dual objects.")
  //  }
}