package net.tqft.toolkit.algebra.khovanov

case class Crossing(sign: Int, strands: (Int, Int, Int, Int))
case class Tangle(crossings: Seq[Crossing])
case class Cobordism()

case class AnnularTangle(cut: Seq[Int], tangle: Tangle)
case class AnnularCobordism()

object Types {
  type O1 = GradedSmoothing
  type M1 = LinearComboCan
  type O2 = Seq[O1]
  type M2 = Matrix[O1, M1]
  type O3 = Complex[O2, M2]
  type M3 = ChainMap[O2, M2]
  type O4 = Seq[O3]
  type M4 = Matrix[O3, M3]
}

