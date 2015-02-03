package net.tqft.toolkit.algebra.matrices2

import scala.language.higherKinds
import net.tqft.toolkit.algebra._

trait Matrix[T[_]] {
  def numberOfRows(t: T[_]): Int
  def numberOfColumns(t: T[_]): Int
  def diagonalEntries[F](t: T[F]): Seq[F]

  def pivotRowColumnAndEntry[F: OrderedField](t: T[F]): Option[(Int, Int, F)]
}

object Matrix {

  type SeqSeq[T] = IndexedSeq[IndexedSeq[T]]

  implicit object SeqMatrix extends Matrix[SeqSeq] {
    override def numberOfRows(t: SeqSeq[_]) = t.size
    override def numberOfColumns(t: SeqSeq[_]) = t.head.size // this explodes for zero row matrices, but that's the price of not using a wrapper
    override def diagonalEntries[F](t: SeqSeq[F]) = {
      for ((r, i) <- t.zipWithIndex) yield r(i)
    }

    override def pivotRowColumnAndEntry[F: OrderedField](t: SeqSeq[F]) = {
      import net.tqft.toolkit.arithmetic.MinMax._
      val r = t.indexAndValueOfMinBy(row => row.zipWithIndex.find(p => p._1 != implicitly[Zero[F]].zero))
      r._2.map(p => (r._1, p._2, p._1))
    }
  }

}

object Matrices {
  def ofSize[R: Ring](n: Int): Monoid[Seq[Seq[R]]] = new Monoid[Seq[Seq[R]]] {
    private val ring = implicitly[Ring[R]]
    override def one = IndexedSeq.tabulate(n, n)({ (i, j) => if (i == j) ring.one else ring.zero })
    override def multiply(x: Seq[Seq[R]], y: Seq[Seq[R]]) = {
      IndexedSeq.tabulate(n, n)({ (i, j) =>
        ring.sum(for (k <- 0 until n) yield ring.multiply(x(i)(k), y(k)(j)))
      })
    }
  }
}

object GaussianElimination {

  implicit class GaussianEliminationOperationsForIndexedSeqMatrix[F: OrderedField](matrix: IndexedSeq[IndexedSeq[F]]) extends GaussianEliminationOperations[F, Matrix.SeqSeq](matrix)

  implicit class GaussianEliminationOperations[F: OrderedField, T[_]: Matrix](matrix: T[F]) {
    private def field = implicitly[OrderedField[F]]
    private def matrices = implicitly[Matrix[T]]

    def rowEchelonForm: T[F] = {
      matrices.pivotRowColumnAndEntry(matrix) match {
        case None => matrix
        case Some((row, column, entry)) => {
          ???
        }
      }
    }
    def determinant: F = field.product(matrices.diagonalEntries(rowEchelonForm))
  }

}