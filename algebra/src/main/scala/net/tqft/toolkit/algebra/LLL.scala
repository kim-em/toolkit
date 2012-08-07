package net.tqft.toolkit.algebra

object LLL {
  def apply(lattice: Seq[Seq[BigInt]]) = {
    val array = lattice.transpose.map(_.map(_.bigInteger).toArray).toArray
    net.tqft.toolkit.algebra.impl.LLL.integral_LLL(array, lattice.size, lattice.head.size)
    array.toSeq.map(_.toSeq.map(new BigInt(_))).transpose
  }
}