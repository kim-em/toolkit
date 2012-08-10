package net.tqft.toolkit.algebra

object Unknowns { 

  private class RingWithUnknowns[A](ring: Ring[A]) extends Ring[Option[A]]{
    override def negate(x: Option[A]) = x.map(ring.negate)
    override def fromInt(x: Int) = Some(ring.fromInt(x))
    override def zero = Some(ring.zero)
    override def one = Some(ring.one)
    override def add(x: Option[A], y:Option[A]) = {
      for(xx <- x; yy <- y) yield ring.add(xx,yy)
    }
    override def multiply(x: Option[A], y:Option[A]) = {
      (x,y ) match {
        case (x, _) if x == ring.zero => Some(ring.zero)
        case (_, y) if y == ring.zero => Some(ring.zero)
        case (None, _) => None
        case (_, None) => None
        case (Some(xx), Some(yy)) => Some(ring.multiply(xx, yy))
      }
    }
  }
  
  def apply[A](ring: Ring[A]): Ring[Option[A]] = new RingWithUnknowns[A](ring)
  
  def apply[A](fusionRing: FusionRing[A]): FusionRing[Option[A]] = new FusionRing[Option[A]] {
    val rank = fusionRing.rank
    def coefficients = Unknowns(fusionRing.coefficients)
    override def multiply(x: Seq[Option[A]], y:Seq[Option[A]]) = {
      def unitVector(k: Int) = Seq.tabulate(rank)(i => if(i == k) fusionRing.coefficients.one else fusionRing.coefficients.zero)
      add(for((xi, i) <- x.zipWithIndex; (yj, j) <- y.zipWithIndex) yield {
        val z = coefficients.multiply(xi, yj)
        fusionRing.multiply(unitVector(i), unitVector(j)).map(s => coefficients.multiply(Some(s), z))
      })
    }
  }
  
}