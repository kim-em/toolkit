package net.tqft.toolkit.algebra.khovanov

trait GaussianElimination[O, M] {
  def locatePseudoIsomorphism(m: Matrix[O, M]): Option[(Int, Int)]
  def pseudoInverse(m: M): M
  def invertible_?(m: M): Boolean

  def oneStep(c: Complex[O, M]): Option[(Int, Complex[O, M], Option[Complex[O, M]])] = ???
  def decompose(c: Complex[O, M]): Seq[Complex[O, M]] = {
    def recursivelyDecompose(summands: Seq[Complex[O, M]], height: Int, remaining: Complex[O, M]): Seq[Complex[O, M]] = {
      oneStep(remaining) match {
        case None => remaining +: summands
        case Some((newHeight, stillRemaining, optionalNewSummand)) => recursivelyDecompose(summands ++ optionalNewSummand, newHeight, stillRemaining)
      }
    }
    recursivelyDecompose(Seq.empty, c.homologicalHeight, c)
  }
}

