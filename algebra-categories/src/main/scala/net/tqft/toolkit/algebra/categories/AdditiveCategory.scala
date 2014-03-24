package net.tqft.toolkit.algebra.categories

import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.Subtractive

trait AdditiveCategory[O, M] extends NLinearCategory[O, M] with Subtractive[M] { ac =>
  protected class EndomorphismRing(o: O) extends EndomorphismRig(o) with Ring[M] {
    override def negate(x: M) = ac.negate(x)
  }

  override def endomorphisms(o: O): Ring[M] = new EndomorphismRing(o)
}
