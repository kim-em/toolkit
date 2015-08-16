package net.tqft.toolkit.algebra.khovanov

/**
 * @author scott
 */

trait Simplification[O, M] {
  def simplify(o: O): Seq[(M, M)] // for each pair (f, g), f and g are inverses, with source(f) = o

  def simplifyMorphism(m: M)(implicit category: Category[O, M]): M = {
    val (_, f) = fullSimplify(category.source(m))
    val (g, _) = fullSimplify(category.target(m))
    category.compose(category.compose(f, m), g)
  }
  
  def fullSimplify(o: O)(implicit category: Category[O, M]): (M, M) = {
    simplify(o).headOption match {
      case None => (category.identity(o), category.identity(o))
      case Some((f, g)) => {
        val p = category.target(f)
        val (s, t) = fullSimplify(p)
        (category.compose(f, s), category.compose(t, g))
      }
    }
  }
}

object Simplification {
  implicit def gaussianElimination[O, M](implicit inner: Simplification[O, M], factorisableCategory: Factorisation[O, M]): Simplification[Complexes[O, M], ChainMaps[O, M]] = {
    new Simplification[Complexes[O, M], ChainMaps[O, M]] {
      override def simplify(c: Complexes[O, M]) = {
        // in each summand complex, look for the first homological height in which a matrix contains something suitably factorisable, ...
        ???
      }
    }
  }
}
