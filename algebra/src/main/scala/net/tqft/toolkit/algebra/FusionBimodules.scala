package net.tqft.toolkit.algebra

object FusionBimodules {
  def commutants(leftModule: FusionRing[Int]#FusionModule, otherRank: Int, otherNumberOfSelfDualObjects: Int, knownBimodule: Option[FusionBimodule[Int]]): Iterable[FusionBimodule[Int]] = {
    // first, make a FusionBimodule with variable entries

    val leftRing = leftModule.fusionRing
    type V = (Int, Int, Int, Int)

    implicit val polynomialAlgebra = MultivariablePolynomialAlgebras.over[Int, V]

    val fusionRingUnknowns = for (i <- 0 until otherRank) yield {
      Matrix(otherRank,
        for (j <- 0 until otherRank) yield {
          for (k <- 0 until otherRank) yield polynomialAlgebra.monomial((0, i, j, k))
        })
    }
    val fusionModuleUnknowns = for (i <- 0 until leftModule.rank) yield {
      Matrix(leftModule.rank,
        for (j <- 0 until otherRank) yield {
          for (k <- 0 until leftModule.rank) yield polynomialAlgebra.monomial((1, i, j, k))
        })

    }

    val leftRingStructureCoefficients = leftRing.structureCoefficients.map(_.mapEntries(x => polynomialAlgebra.constant(x)))
    val leftModuleStructureCoefficients = leftModule.structureCoefficients.map(_.mapEntries(x => polynomialAlgebra.constant(x)))

    val variableBimodule = FusionBimodule(leftRingStructureCoefficients, leftModuleStructureCoefficients, fusionRingUnknowns, fusionModuleUnknowns)

    val otherDuality = (0 until otherNumberOfSelfDualObjects) ++ (for (i <- otherNumberOfSelfDualObjects until otherRank by 2; j <- Seq(i + 1, i)) yield j)
    require(otherDuality.size == otherRank)

    val knownSolution = knownBimodule.map({ bimodule => 
      val rrsc =bimodule.rightRing.structureCoefficients
      val rmsc =bimodule.rightModule.structureCoefficients
    	((for(i <- 0 until otherRank; j <- 0 until otherRank; k <- 0 until otherRank) yield (0,i,j,k)->rrsc(i).entries(j)(k)) ++
    	(for(i <- 0 until leftModule.rank; j <- 0 until otherRank; k <- 0 until leftModule.rank) yield (1,i,j,k)->rmsc(i).entries(j)(k))).toMap
    })
    
    val (solutions, tooHard) = IntegerPolynomialProgramming.solve(
      (variableBimodule.associativityConstraints.flatten ++ variableBimodule.identityConstraints.flatten ++ variableBimodule.rightRing.dualityConstraints(otherDuality).flatten).toSeq, knownSolution)
      
    // failing here, because, well, the problem is too hard as specified. 
      
    require(tooHard.isEmpty)

    for (solution <- solutions) yield {
      val rightRingStructureCoefficients = fusionRingUnknowns.map(_.mapEntries({
        case p if p.totalDegree == Some(1) => solution(p.terms.head._1.keySet.iterator.next)
        case p => p.constantTerm
      }))
      val rightModuleStructureCoefficients = fusionModuleUnknowns.map(_.mapEntries({
        case p if p.totalDegree == Some(1) => solution(p.terms.head._1.keySet.iterator.next)
        case p => p.constantTerm
      }))
      FusionBimodule(leftRing.structureCoefficients, leftModule.structureCoefficients, rightRingStructureCoefficients, rightRingStructureCoefficients)
    }
  }

}