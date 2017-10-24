package net.tqft.toolkit.algebra.matrices

import scala.collection.GenSeq

class AbstractDenseCategoricalMatrix[A, B, M <: AbstractDenseCategoricalMatrix[A, B, M]](sources: Seq[A], targets: Seq[A], val entries: GenSeq[Seq[B]]) extends CategoricalMatrix[A, B, M](sources, targets) {
  override def equals(other: Any) = {
    other match {
      case other: AbstractDenseCategoricalMatrix[_, _, _] => {
        sources == other.sources &&
          targets == other.targets &&
          entries == other.entries
      }
      case _ => false
    }
  }
  override def lookupEntry(row: Int)(column: Int) = entries(row)(column)
}
