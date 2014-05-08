package net.tqft.toolkit.algebra.combinatorics

import net.tqft.toolkit.algebra.numberfields.NumberField
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.AlgebraicNotation
import net.tqft.toolkit.algebra.polynomials.PolynomialsOverIntegerModel
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.numberfields.MinimalPolynomial
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.matrices.CholeskyDecomposition

object PositiveSymmetricDecomposition2 {

  def apply[I: IntegerModel](
    m: Seq[Seq[Int]],
    numberField: NumberField[Fraction[I]],
    //    numericNumberFieldGenerator: Double,
    globalDimension: Polynomial[Fraction[I]],
    dimensions: Seq[Polynomial[Fraction[I]]]): Iterator[Seq[Seq[Int]]] = {

    val rank = m.size

    import IntegerModel._
    import AlgebraicNotation._

    //    val numericGlobalDimension = globalDimension.mapValues(_.toDouble).evaluateAt(numericNumberFieldGenerator)
    //    val numericDimensions = dimensions.map(_.mapValues(_.toDouble).evaluateAt(numericNumberFieldGenerator))

    val polynomials = implicitly[PolynomialsOverIntegerModel[I]]

    def positive_?(matrix: Seq[Seq[Int]]): Boolean = {
      import CholeskyDecomposition._
      val cd = Matrix(rank, matrix.map(r => r.map(x => x.toDouble + 0.0001))).choleskyDecomposition
      println(cd)
      cd.nonEmpty
    }

    def subtractColumn(matrix: Seq[Seq[Int]], column: Seq[Int]) = {
      matrix.zip(column).map(r => r._1.zip(column).map(s => s._1 - r._2 * s._2))
    }

    val allowedColumns: Seq[List[Int]] = {
      // prepare all possible columns with dimension < globalDimension

      case class PartialColumn(column: List[Int], dimension: Polynomial[Fraction[I]], reversedDimensions: List[Polynomial[Fraction[I]]], reversedDiagonalEntries: List[Int]) {
        //        println(s"PartialColumn($column, $dimension, $reversedDiagonalEntries)")
        def children: Iterator[PartialColumn] = {

          // we could remove many more if needed, by ensuring that all entries stay positive
          for (i <- (0 to scala.math.sqrt(reversedDiagonalEntries.head + 0.0001).toInt).iterator) yield {
            PartialColumn(
              i :: column,
              dimension + numberField.multiplyByInt(reversedDimensions.head, i),
              reversedDimensions.tail,
              reversedDiagonalEntries.tail)
          }
        }
        def completions: Iterator[(Polynomial[Fraction[I]], List[Int])] = {
          if (reversedDiagonalEntries.nonEmpty) {
            children.flatMap(_.completions)
          } else {
            Iterator((dimension, column))
          }
        }
      }

      val allColumns = {
        val iterator = PartialColumn(Nil, polynomials.zero, dimensions.reverse.toList, m.zipWithIndex.map(p => p._1(p._2)).reverse.toList).completions
        // we need to dump the zero column
        iterator.next
        iterator.toStream
      }

      val smallColumns = allColumns.filter(c => positive_?(subtractColumn(m, c._2)))
      println("Found " + allColumns.size + " columns")
      println(" ... of those " + smallColumns.size + " are small enough")

      // filter out only those where the dimension doesn't divide globalDimension as an algebraic integer
      // filter out those where the dimension is not a d-number
      val result = smallColumns.toSeq.groupBy(_._1).filterKeys({ c =>
        import MinimalPolynomial._
        println("considering dimension: " + c)
        c != numberField.zero &&
          numberField.integer_?(numberField.quotient(globalDimension, c)) &&
          numberField.dNumber_?(c)
      }).values.flatMap(_.map(_._2)).toStream

      println(" ... and " + result.size + " pass the number theoretic tests")
      result
    }

    sealed trait TreeNode
    case class Leaf(column: List[Int]) extends TreeNode
    case class Branch(depth: Int, map: List[(Int, TreeNode)]) extends TreeNode {
      override def toString = map.map(_.toString.replace("\n", "\n  ")).mkString("branch(\n  ", ",\n  ", "\n)")
    }

    object TreeNode {
      def apply(columns: Seq[List[Int]], i: Int = 0): TreeNode = {
        if (columns.size == 1) {
          Leaf(columns.head)
        } else {
          Branch(i, columns.groupBy(_(i)).mapValues(s => apply(s, i + 1)).toList.sortBy(_._1))
        }
      }
    }

    val tree = TreeNode(allowedColumns)

    //    println(tree)

    import Ordering.Implicits._

    def treeIterator(tree: TreeNode, nonzeroEntryIndex: Int, lexicographicUpperBound: Option[List[Int]], entryUpperBounds: Seq[Int]): Iterator[List[Int]] = {
      tree match {
        case Leaf(column) => {
          if ((lexicographicUpperBound.isEmpty || column <= lexicographicUpperBound.get) && column(nonzeroEntryIndex) > 0) {
            Iterator(column)
          } else {
            Iterator.empty
          }
        }
        case Branch(i, branches) => branches.iterator.flatMap({ b =>
          if (i == nonzeroEntryIndex && b._1 == 0 || b._1 > entryUpperBounds(i)) {
            Iterator.empty
          } else {
            if (lexicographicUpperBound.isEmpty || b._1 < lexicographicUpperBound.get(i)) {
              treeIterator(b._2, nonzeroEntryIndex, None, entryUpperBounds)
            } else if (b._1 == lexicographicUpperBound.get(i)) {
              treeIterator(b._2, nonzeroEntryIndex, lexicographicUpperBound, entryUpperBounds)
            } else {
              Iterator.empty
            }
          }
        })
      }
    }

    case class PartialDecomposition(difference: Seq[Seq[Int]], columns: List[List[Int]]) {
      println(this)
      require(positive_?(difference))
      require(difference.forall(_.forall(_ >= 0)))
      lazy val firstNonzeroEntry = {
        var k = 0
        while (difference(k)(k) == 0) {
          k = k + 1
        }
        k
      }
      // TODO cache this?
      lazy val differenceDiagonalSqrts: Seq[Int] = {
        for (k <- 0 until rank) yield scala.math.sqrt(difference(k)(k) + 0.0001).toInt
      }
      def randomDecomposition: (Option[List[List[Int]]], Int) = {
        if (difference.forall(_.forall(_ == 0))) {
          (Some(columns), 1)
        } else {
          val nextColumns = addOneColumn.toSeq
          if (nextColumns.isEmpty) {
            (None, 1)
          } else {
            val next = nextColumns(scala.util.Random.nextInt(nextColumns.size)).randomDecomposition
            (next._1, next._2 * nextColumns.size)
          }
        }
      }
      def addOneColumn: Iterator[PartialDecomposition] = {
        // TODO: filter out some very basic inequalities (ensuring all the entries remain positive)
        def nextColumns: Iterator[List[Int]] = {
          treeIterator(tree, firstNonzeroEntry, columns.headOption, differenceDiagonalSqrts)
        }

        // filter out by positivity, using the Cholesky decomposition
        nextColumns.flatMap({ c =>
          val newDifference = subtractColumn(difference, c)
          if (newDifference.zipWithIndex.forall(p => p._1(p._2) > 0 || p._1.forall(_ == 0))) {
            if (positive_?(newDifference)) {
              Some(PartialDecomposition(
                newDifference,
                c :: columns))
            } else {
              None
            }
          } else {
            None
          }
        })
      }
      def addColumns: Iterator[List[List[Int]]] = {
        if (difference.forall(_.forall(_ == 0))) {
          Iterator(columns)
        } else {
          addOneColumn.flatMap(_.addColumns)
        }
      }
    }

    for (i <- 0 until 100) println(PartialDecomposition(m, Nil).randomDecomposition._2)

    PartialDecomposition(m, Nil).addColumns
  }

}