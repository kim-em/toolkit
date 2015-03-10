package net.tqft.toolkit.algebra.matrices

import net.tqft.toolkit.algebra.ApproximateReals

object CholeskyDecomposition {

  implicit class MatrixCholeskyDecomposition[B: ApproximateReals](matrix: Matrix[B]) {
    val field = implicitly[ApproximateReals[B]]

    def positiveSemidefinite_? : Boolean = {
      choleskyDecomposition.nonEmpty
    }

    def choleskyDecomposition: Option[Matrix[B]] = {
      require(matrix.numberOfColumns == matrix.numberOfRows)

      import net.tqft.toolkit.functions.Memo

      lazy val L: (Int, Int) => B = Memo({ (i: Int, j: Int) =>
        val result = if (i == j) {
          diagonalEntries(i).get
        } else if (i > j) {
          // what to do when the diagonal entries are zero?
          // http://arxiv.org/pdf/1202.1490.pdf
          if (field.compare(L(j, j), field.zero) == 0) {
            field.zero
          } else {
            field.quotient(field.subtract(matrix.entries(i)(j), field.sum(for (k <- 0 until j) yield field.multiply(L(i, k), L(j, k)))), L(j, j))
          }
        } else {
          field.zero
        }
        //      println(i + " " + j + " " + result)
        result
      })

      lazy val diagonalEntries: Int => Option[B] = Memo({ i: Int =>
        val r = field.subtract(matrix.entries(i)(i), field.sum(for (k <- 0 until i) yield field.power(L(i, k), 2)))
        val c = field.compare(field.chop(r), field.zero)
        val result = if (c < 0) {
          None
        } else if (c == 0) {
          Some(field.zero)
        } else {
          Some(field.sqrt(r))
        }
        //      println(i + " " + result)
        result
      })

      if ((0 until matrix.numberOfColumns).forall(i => diagonalEntries(i).nonEmpty)) {
        Some(Matrix(matrix.numberOfColumns, Seq.tabulate(matrix.numberOfColumns, matrix.numberOfColumns)({ (i, j) => L(i, j) })))
      } else {
        None
      }
    }

  }
}