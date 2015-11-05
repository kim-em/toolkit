package net.tqft.toolkit.algebra.khovanov

/**
 * @author scott
 */

trait FindIsomorphism[O, M] {
  def findIsomorphism(o1: O, o2: O): Option[(M, M)]
}

object FindIsomorphism {
  implicit def findPermutationIsomorphism[O, M](implicit category: Category[O, M]): FindIsomorphism[DirectSum[O], Matrix[O, M]] = {
    new FindIsomorphism[DirectSum[O], Matrix[O, M]] {
      override def findIsomorphism(c1: DirectSum[O], c2: DirectSum[O]) = {
        // looking for a permutation under which everything matches up on the nose
        ???
      }      
    }    
  }
  
  implicit def findIsomorphismBetweenComplexes[O, M](implicit inner: FindIsomorphism[O, M]): FindIsomorphism[Complex[O, M], ChainMap[O, M]] = {
    new FindIsomorphism[Complex[O, M], ChainMap[O, M]] {
      override def findIsomorphism(c1: Complex[O, M], c2: Complex[O, M]) = {
        // go along the homological heights, looking for isomorphisms
        ???
      }      
    }
  }
  def using[O, M](simplification: Simplification[O, M])(implicit category: Category[O, M], inner: FindIsomorphism[O, M]): FindIsomorphism[O, M] = {
    new FindIsomorphism[O, M] {
      override def findIsomorphism(o1: O, o2: O) = {
        val (f1, g1) = simplification.fullSimplify(o1)
        val (f2, g2) = simplification.fullSimplify(o2)
        inner.findIsomorphism(category.target(f1), category.target(f2)) match {
          case None => None
          case Some((a, b)) => {
            Some((
              category.compose(category.compose(f1, a), g2),
              category.compose(category.compose(f2, b), g1)))
          }
        }
      }
    }
  }
}
