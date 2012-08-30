package net.tqft.toolkit.algebra

object LLL {
  def apply[I:IntegerModel](lattice: Seq[Seq[I]]): Seq[Seq[I]] = {
    val integers = implicitly[IntegerModel[I]]
    applyImpl(lattice.map(_.map(integers.toBigInt _))).map(_.map(integers.fromBigInt _))
  }
  
  private def applyImpl(lattice: Seq[Seq[BigInt]]): Seq[Seq[BigInt]] = {
    val array = lattice.transpose.map(_.map(_.bigInteger).toArray).toArray
    net.tqft.toolkit.algebra.impl.LLL.integral_LLL(array, lattice.size, lattice.head.size)
    array.toSeq.map(_.toSeq.map(new BigInt(_))).transpose
  }
}