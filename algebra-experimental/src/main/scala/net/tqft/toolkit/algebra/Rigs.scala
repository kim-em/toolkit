package net.tqft.toolkit.algebra

object Unknowns {
  sealed trait ?
  case object ? extends ?
}

object Rigs {
  import Unknowns.?
  import net.tqft.toolkit.UnionTypes._
  
  def adjoinUnknown[A: Rig]: Rig[A or ?] = {
    val rig = implicitly[Rig[A]]
    new Rig[A or ?] {
      override def zero = rig.zero
      override def one = rig.one
      override def fromInt(k: Int) = rig.fromInt(k)
      override def fromBigInt(k: BigInt) = rig.fromBigInt(k)
      override def add(x: A or ?, y: A or ?) = for (xa <- x.left; ya <- y.left) yield rig.add(xa, ya)
      override def multiply(x: A or ?, y: A or ?) = {
        if(x == Left(rig.zero) || y == Left(rig.zero)) {
          rig.zero
        } else {
          for (xa <- x.left; ya <- y.left) yield rig.multiply(xa, ya)
        }
      }
    }
  }  
}