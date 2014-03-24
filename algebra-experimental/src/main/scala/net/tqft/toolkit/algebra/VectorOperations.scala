package net.tqft.toolkit.algebra
import net.tqft.toolkit.collections.SparseSeq
import scala.collection.mutable.WrappedArray

object VectorOperations extends net.tqft.toolkit.Logging {
  var issuedVolatileWarning = false
  def warn {
    if (!issuedVolatileWarning) {
      warn("VectorOperations mutates in place when given WrappedArray arguments. Be careful!")
      issuedVolatileWarning = true
    }
  }

  def tensor[A: Rig](v1: Seq[A], v2: Seq[A]): Seq[A] = {
    val rig = implicitly[Rig[A]]
    if (v1.isInstanceOf[SparseSeq[_]] || v2.isInstanceOf[SparseSeq[_]]) {
      val s1 = SparseSeq.from(v1, rig.zero)
      val s2 = SparseSeq.from(v2, rig.zero)
      val entries = (for ((a1, k1) <- s1.nonZeroEntries; (a2, k2) <- s2.nonZeroEntries) yield (rig.multiply(a1, a2), v2.size * k1 + k2))
      val filteredEntries = if (rig.isInstanceOf[DivisionRing[_]]) {
        entries
      } else {
        entries filter ({ case (a, k) => a != rig.zero })
      }
      SparseSeq.withEntries(v1.size * v2.size, filteredEntries, rig.zero)
    } else {
      for (x1 <- v1; x2 <- v2) yield rig.multiply(x1, x2)
    }
  }

  def scalarMultiply[A: Rig](a: A, v: Seq[A]): Seq[A] = {
    val rig = implicitly[Rig[A]]
    if (v.isInstanceOf[WrappedArray[_]]) {
      warn
      val array = v.asInstanceOf[WrappedArray[A]]
      for (i <- 0 until array.size) {
        array(i) = rig.multiply(a, array(i))
      }
      array
    } else if (v.isInstanceOf[SparseSeq[_]]) {
      val s = v.asInstanceOf[SparseSeq[A]]
      SparseSeq.withEntries(v.size, for ((b, i) <- s.asInstanceOf[SparseSeq[A]].nonZeroEntries; r = rig.multiply(a, b); if (r != rig.zero)) yield (r, i), rig.zero)
    } else {
      v map { x => rig.multiply(a, x) }
    }
  }

  def add[A: AdditiveMonoid](v1: Seq[A], v2: Seq[A]): Seq[A] = {
    require(v1.size == v2.size)

    val monoid = implicitly[AdditiveMonoid[A]]
    if (v1.isInstanceOf[WrappedArray[_]] && v2.isInstanceOf[SparseSeq[_]]) {
      warn
      val array = v1.asInstanceOf[WrappedArray[A]]
      val e2 = v2.asInstanceOf[SparseSeq[A]].nonZeroEntries
      for ((a2, k2) <- e2) {
        array(k2) = monoid.add(array(k2), a2)
      }
      array
    } else if (v1.isInstanceOf[WrappedArray[_]]) {
      warn
      val array = v1.asInstanceOf[WrappedArray[A]]
      for ((a, k) <- v2.zipWithIndex) {
        array(k) = monoid.add(array(k), a)
      }
      array
    } else if (v1.isInstanceOf[SparseSeq[_]] && v2.isInstanceOf[SparseSeq[_]]) {
      val e1 = v1.asInstanceOf[SparseSeq[A]].nonZeroEntries
      val e2 = v2.asInstanceOf[SparseSeq[A]].nonZeroEntries
      val entries = (e1 ++ e2).groupBy(_._2).mapValues({ v => monoid.sum(v.map(_._1)) }).toSeq.map(_.swap).filter(_._1 != monoid.zero)
      SparseSeq.withEntries(v1.size, entries, monoid.zero)
    } else {
      (v1 zip v2) map { p => monoid.add(p._1, p._2) }
    }
  }
}