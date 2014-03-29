package net.tqft.toolkit.algebra

trait VectorSpace[F, V] extends Module[F, V] {
  def rank: Int
  def coefficients: Field[F]
}

object VectorSpace {
  
}