package net.tqft.toolkit.algebra.matrices

import scala.collection.GenSeq
import net.tqft.toolkit.algebra.Field

abstract class CategoricalMatrix[A, B, M <: CategoricalMatrix[A, B, M]](val sources: Seq[A], val targets: Seq[A]) {
  require(targets.size == entries.size)
  require(entries.find(_.size != numberOfColumns).isEmpty) // not a good idea if the entries are in Hadoop

  def entries: GenSeq[Seq[B]]
  def lookupEntry(row: Int)(column: Int): B
  def pivotPosition(row: Int, ignoring: B): Option[Int]

  def numberOfColumns = sources.size
  def numberOfRows = entries.size

  def findBasisForRowSpace(rankBound: Option[Int] = None)(implicit field: Field[B]): List[Int] = {
    case class Progress(rowsConsidered: Int, basis: List[Int], reducedRows: Option[Matrix[B]])

    def considerAnotherRow(soFar: Progress, row: Seq[B]): Progress = {
      if (rankBound == Some(soFar.basis.size)) {
        // we've found enough, short circuit
        soFar
      } else {
        val next = (soFar.reducedRows match {
          case None => Matrix(row.size, List(row))
          case Some(m) => m.appendRow(row)
        }).rowEchelonForm

        if (next.numberOfRows > 0 && next.pivotPosition(next.numberOfRows - 1, field.zero).nonEmpty) {
          Progress(soFar.rowsConsidered + 1, soFar.rowsConsidered :: soFar.basis, Some(next))
        } else {
          Progress(soFar.rowsConsidered + 1, soFar.basis, soFar.reducedRows)
        }
      }
    }

    entries.foldLeft(Progress(0, List(), None))(considerAnotherRow _).basis.reverse

  }

  def rank(rankBound: Option[Int] = None)(implicit field: Field[B]): Int = findBasisForRowSpace(rankBound).size

}
