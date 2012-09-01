package net.tqft.toolkit.algebra.lattices

import net.tqft.toolkit.algebra.Gadgets
import net.tqft.toolkit.algebra.IntegerModel

object LLL {
  def apply[I: IntegerModel](lattice: Seq[Seq[I]]): Seq[Seq[I]] = {
    val integers = implicitly[IntegerModel[I]]
    if (integers == Gadgets.BigIntegers) {
      applyImpl(lattice.asInstanceOf[Seq[Seq[BigInt]]]).asInstanceOf[Seq[Seq[I]]]
    } else {
      applyImpl(lattice.map(_.map(integers.toBigInt _))).map(_.map(integers.fromBigInt _))
    }
  }

  private def applyImpl(lattice: Seq[Seq[BigInt]]): Seq[Seq[BigInt]] = {
    val array = lattice.transpose.map(_.map(_.bigInteger).toArray).toArray
    net.tqft.toolkit.algebra.lattices.impl.LLL.integral_LLL(array, lattice.size, lattice.head.size)
    array.toSeq.map(_.toSeq.map(new BigInt(_))).transpose
  }
}