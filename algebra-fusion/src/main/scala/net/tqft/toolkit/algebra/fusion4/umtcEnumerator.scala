package net.tqft.toolkit.algebra.fusion4

case class SmallGroup(order: Int, index: Int)
case class GxGOrbit(name: Int, index: Int)
case class SelfDualOrbit(orbit: GxGOrbit, dual: (Int, Int)) // X^* = dual._1 X dual._2
case class DualPairOrbit(orbit1: GxGOrbit, orbit2: GxGOrbit) // X^* = Y

case class UMTCEnumerator(invertibles: SmallGroup) {

  case class OrbitStructure(selfDualOrbits: Seq[SelfDualOrbit], dualPairOrbits: Seq[DualPairOrbit])

}

case class SymmetricMatrixEnumerator(
    initialMatrix: Array[Array[Int]],
    eigenvalueBound: Double,
    coefficientClusters: Array[List[(Int, Int)]]) extends Iterator[Array[Array[Int]]] {

  val rank = initialMatrix.length
  var hint = Array.fill(rank)(1.0)
  var cluster = 0
  val numberOfClusters = coefficientClusters.length
  var done = false
  
  def hasNext = {
    ???
  }
  def next = {
    ???
  }

}