package net.tqft.toolkit.algebra.fusion

object FusionRingSandbox extends App {

  val H1 = FusionRings.Examples.H1
  val AH1 = FusionRings.Examples.AH1
  val AH2 = FusionRings.Examples.AH2
  val AH3 = FusionRings.Examples.AH3

  H1.verifyDuality()
  H1.verifyDuality(IndexedSeq(0, 1, 2, 3))

  val H1r = H1.regularModule.ensuring(_.verifyAdmissibility)
  val bm = FusionBimoduleWithLeftDimensions(H1.regularModule, H1.structureCoefficients, H1r.structureCoefficients).ensuring(_.verifyAssociativity).ensuring(_.verifyAdmissibility).ensuring(_.verifyIdentity)

//  for (fm <- H1.candidateFusionModules) {
//    println(fm.verifyAdmissibility)
//    println(H1.FusionModules.equivalent_?(fm, H1r))
//  }
  //  println(FusionBimodules.commutants(H1r, rankBound=Some(6)).size)

//  for (fm <- H1.candidateFusionModules) {
//    val commutants = FusionBimodules.commutants(fm, rankBound = Some(6))
//    println(fm + " has " + commutants.size + " commutants with rank at most 6.")
//  }

//  println("AH1 has " + AH1.candidateFusionModules.size + " modules")
  println("AH2 has " + AH2.candidateFusionModules.size + " modules")
  println("AH3 has " + AH3.candidateFusionModules.size + " modules")
  
//  for (fm <- AH1.candidateFusionModules) {
//    val commutants = FusionBimodules.commutantsWithRank(fm, 9, 7)
//    println(fm + " has " + commutants.size + " commutants with rank 9 and 7 self-dual objects.")
//  }
  
//  val rings = AH1.candidateFusionModules.flatMap(fm => FusionBimodules.commutantsWithRank(fm, 9, 7)).map(_.rightRing).toSeq
//  println(rings.size + " commutants")
//  for(ring <- rings) {
//    println(ring.structureCoefficients)
//  }

  
  //  for (fm <- H1.candidateFusionModules; b <- FusionBimodules.commutants(fm, 4, 4, None)) {
  //    println(b.rightRing.structureCoefficients)
  //  }

  //    for (r <- FusionRings.withObject(AH1.structureCoefficients(1)); m <- r.structureCoefficients) { println(m); println() }

  def test(G: FusionRingWithDimensions) {
    println("Start: " + new java.util.Date())
    println("dimension bounds: " + G.basis.map(G.dimensionUpperBounds))
    println(G.candidateAlgebraObjects.toList)
    for (m <- G.candidateFusionMatrices) {
      println(m.algebraObject)
      println(m.matrix)
      println(m.dimensionsSquared)
      println(m.dimensionsSquared.map(G.dimensionField.approximateWithin(0.001)))
    }
    println("Finish: " + new java.util.Date())
    //    println(G.candidateFusionModules.size)
    var count = 0
    for (fm <- G.candidateFusionModules) {
      println(fm.structureCoefficients)
      count = count + 1
      println("found " + count + " modules so far")
    }
  }
  //  test(H1)
  //  test(AH1)

  //  val v = Seq(1, 1, 2, 3, 3, 4)
  ////  val v = Seq(1,2,3,4,5,7)
  //  println(AH1.objectsSmallEnoughToBeAlgebras.contains(v))
  //  println(AH1.smallObjectsWithPositiveSemidefiniteMultiplication.contains(v))
  //  println(AH1.regularModule.asMatrix(v));
  //  {
  //    import Implicits.Rationals
  //    println(AH1.regularModule.asMatrix(v).mapEntries(Implicits.integersAsRationals).positiveSemidefinite_?)
  //  }
  //  println(Matrices.positiveSymmetricDecompositions(AH1.regularModule.asMatrix(v)).toList)
  //  haagerupFusionRing.candidateBrauerPicardGroupoids
}