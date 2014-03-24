package net.tqft.toolkit.algebra.matrices

import scala.collection.GenSeq
import net.tqft.toolkit.algebra.Field

abstract class CategoricalMatrix[A, B, M <: CategoricalMatrix[A, B, M]](val sources: Seq[A], val targets: Seq[A]) {
//  require(targets.size == entries.size)
//  require(entries.find(_.size != numberOfColumns).isEmpty) // not a good idea if the entries are in Hadoop

  // provide both, to allow for sparse and dense matrices (maybe a bad idea; should be exclusive types?)
  def entries: GenSeq[Seq[B]]
  def lookupEntry(row: Int)(column: Int): B

  def numberOfColumns = sources.size
  def numberOfRows = entries.size
}
