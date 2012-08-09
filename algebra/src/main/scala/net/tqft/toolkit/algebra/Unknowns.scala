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
      // TODO zero should beat None
      for(xx <- x; yy <- y) yield ring.multiply(xx,yy)
    }
  }
  
  def apply[A](ring: Ring[A]): Ring[Option[A]] = new RingWithUnknowns[A](ring)
  
  def apply[A](fusionRing: FusionRing[A]): FusionRing[Option[A]] = new FusionRing[Option[A]] {
    val rank = fusionRing.rank
    def coefficients = Unknowns(fusionRing.coefficients)
    override def multiply(x: Seq[Option[A]], y:Seq[Option[A]]) = {
      ???
    }
  }
  
}