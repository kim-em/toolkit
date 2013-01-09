package net.tqft.toolkit.algebra.graphs

import scala.collection.SeqLike

object NautyHelper {

  import net.tqft.toolkit.collections.Pad._
  def R(x: String): Array[Byte] = {
    x.padTo(x.size % 6 match { case 0 => x.size; case k => x.size + 6 - k }, '0')
      .grouped(6)
      .map(w => (Integer.parseInt(w, 2) + 63).toByte)
      .toArray
  }
  def bigendianBinary(n: Long, k: Int): String = {
    n.toBinaryString.reverse.padTo(k, '0').reverse
  }

  def N(n: Long): Array[Byte] = {
    if (0L <= n && n <= 62L) {
      new Array((n + 63).toByte)
    } else if (63L <= n && n <= 258047L) {
      126.toByte +: R(bigendianBinary(n, 18))
    } else if (258048L <= n && n <= 68719476735L) {
      126.toByte +: 126.toByte +: R(bigendianBinary(n, 36))
    } else {
      throw new IllegalArgumentException
    }
  }
}