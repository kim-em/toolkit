package net.tqft.toolkit.algebra.matrices

import scala.collection.GenSeq
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._

import scala.language.implicitConversions

object Matrix extends net.tqft.toolkit.Logging {
  
  def apply[B](numberOfColumns: Int, entries: GenSeq[Seq[B]]): Matrix[B] = new Matrix(numberOfColumns, entries.map({
    case r: IndexedSeq[B] => r
    case r => r.toIndexedSeq
  }))
  implicit def from[B](entries: GenSeq[Seq[B]]): Matrix[B] = {
    require(entries.nonEmpty)
    apply(entries.head.size, entries)
  }
  def fromArray[B](entries: Array[Array[B]]): Matrix[B] = {
    require(entries.nonEmpty)
    apply(entries(0).length, entries.map(_.toSeq).toSeq)
  }

  // FIXME deprecate this in favour of the pivotPosition defined on CategoricalMatrix
  private def pivotPosition2[B](row: Seq[B])(implicit field: Field[B]): Option[Int] = row.indexWhere { b: B => b != field.zero } match {
    case -1 => None
    case k => Some(k)
  }

  def singleColumn[B](vector: Seq[B]) = new Matrix(1, vector map { x => IndexedSeq(x) })

  def diagonalMatrix[B](vector: Seq[B])(implicit rig: Rig[B]) = new Matrix(vector.size, vector.zipWithIndex map { case (v, k) => (List.fill(k)(rig.zero) ::: List(v) ::: List.fill(vector.size - k - 1)(rig.zero)).toIndexedSeq })

  def identityMatrix[B](size: Int)(implicit rig: Rig[B]) = diagonalMatrix(List.fill(size)(rig.one))

  def tabulate[B](numberOfRows: Int, numberOfColumns: Int)(entries: (Int, Int) => B): Matrix[B] = {
    Matrix(numberOfColumns, IndexedSeq.tabulate(numberOfRows, numberOfColumns)(entries))
  }
}

//private class ConstantSeq[B](b: B, override val size: Int) extends IndexedSeq[B] {
//  override def length = size
//  override def apply(i: Int) = b
//  override def iterator = Iterator.continually(b).take(size)
//}
//
//private class ConstantUnitSeq(size: Int) extends ConstantSeq((), size)

class Matrix[B](
  val numberOfColumns: Int,
  val entries: GenSeq[IndexedSeq[B]]) /* extends AbstractDenseCategoricalMatrix[Unit, B, Matrix[B]](new ConstantUnitSeq(numberOfColumns), new ConstantUnitSeq(entries.size), entries) */ {

  def numberOfRows = entries.size

  def mapRows[C](rowMapper: IndexedSeq[B] => IndexedSeq[C], newColumnSize: Int = numberOfColumns) = new Matrix(newColumnSize, entries.map(rowMapper))
  def mapEntries[C](entryMapper: B => C) = new Matrix(numberOfColumns, entries.map(row => row.map(entryMapper)))

  override def toString = "Matrix(" + numberOfColumns + ", Seq(\n" + entries.map(_.toList).mkString(", \n") + "\n))"
  override def equals(other: Any) = {
    other match {
      case other: Matrix[B] => numberOfColumns == other.numberOfColumns && entries == other.entries
    }
  }
  override def hashCode = numberOfColumns + entries.hashCode

  import Matrix.pivotPosition2

  def transpose = new Matrix(numberOfRows, entries.toIndexedSeq.transpose)

  def takeColumn(column: Int) = entries map { row => row(column) }

  def takeColumns(columns: Seq[Int]) = new Matrix(columns.size, entries map { row => (columns map { i => row(i) }).toIndexedSeq })
  def dropColumns(columns: Seq[Int]) = takeColumns((0 until numberOfColumns) filterNot (columns contains _))

  def takeRows(rows: Seq[Int]) = new Matrix(numberOfColumns, rows map { i => entries(i) })
  def dropRows(rows: Seq[Int]) = takeRows((0 until numberOfRows).toList filterNot (rows contains _))

  def appendRow(row: Seq[B]) = {
    require(row.size == numberOfColumns)
    Matrix(numberOfColumns, entries :+ row)
  }

  def prependRow(row: Seq[B]) = {
    require(row.size == numberOfColumns)
    Matrix(numberOfColumns, row +: entries)
  }

  def joinRows(other: Matrix[B]) = {
    new Matrix(numberOfColumns + other.numberOfColumns, (entries zip other.entries) map { case (r1, r2) => r1 ++ r2 })
  }
  def appendColumn(column: Seq[B]) = {
    new Matrix(numberOfColumns + 1, (entries zip column) map { case (r, c) => r :+ c })
  }
  def prependColumn(column: Seq[B]) = {
    new Matrix(numberOfColumns + 1, (entries zip column) map { case (r, c) => c +: r })
  }

  def permuteColumns(p: IndexedSeq[Int]) = {
    import net.tqft.toolkit.permutations.Permutations._
    new Matrix(numberOfColumns, entries.map(row => p.permute(row)))
  }
  def permuteRows(p: IndexedSeq[Int]) = {
    import net.tqft.toolkit.permutations.Permutations._
    new Matrix(numberOfColumns, p.permute(entries))
  }

  def apply(vector: Seq[B])(implicit rig: Rig[B]) = {
    (for (row <- entries) yield {
      rig.sum(for ((x, y) <- row zip vector) yield rig.multiply(x, y))
    }).seq
  }
  def apply(vector: IndexedSeq[B])(implicit rig: Rig[B]) = {
    (for (row <- entries) yield {
      rig.sum(for (i <- 0 until row.size) yield rig.multiply(row(i), vector(i)))
    }).seq

  }

  def trace(implicit addition: AdditiveMonoid[B]): B = {
    require(numberOfRows == numberOfColumns)
    addition.sum(for (p <- entries.zipWithIndex) yield p._1(p._2))
  }

  private def rowPriority(row: Seq[B])(implicit field: Field[B]): (Int, B) = {
    field match {
      case o: OrderedField[_] => {
        implicit val orderedField = o
        pivotPosition2(row)(field).map(i => (i, field.negate(orderedField.abs(row(i))))).getOrElse((Integer.MAX_VALUE, field.zero))
      }
      case _ => (pivotPosition2(row).getOrElse(Integer.MAX_VALUE), field.zero)
    }
  }

  private def elementOrdering(implicit field: Field[B]): Ordering[B] = {
    field match {
      case field: OrderedField[_] => field
      case _ => new Ordering[B] { def compare(x: B, y: B) = 0 }
    }
  }

  //  private def fieldElementManifest(implicit field: Field[B]): Manifest[B] = ClassManifest.singleType(field.zero.asInstanceOf[B with AnyRef]).asInstanceOf[Manifest[B]]

  private def _rowReduce(rows: GenSeq[Seq[B]], forward: Boolean)(implicit field: Field[B]): GenSeq[Seq[B]] = {
    // we carry around the row indexes separately, because remainingRows might be living off in Hadoop or something...

    //    implicit val bManifest = fieldElementManifest
    //    import net.tqft.toolkit.hadoop.WiredCollections._

    @scala.annotation.tailrec
    def recurse(finishedRows: List[Seq[B]], remainingRows: GenSeq[(Seq[B], Int)], remainingIndexes: Seq[Int]): List[Seq[B]] = {
      
      //      println("finished " + finishedRows.size + " rows")

      if (remainingIndexes.isEmpty) {
        finishedRows.reverse
      } else {

        val (h, targetRow) = if (forward) {
          implicit val BOrdering = elementOrdering
          remainingRows.minBy({ ri: (Seq[B], Int) => rowPriority(ri._1) })
        } else {
          remainingRows.minBy(_._2)
        }

        val rest = remainingRows.filter(_._2 != targetRow)
        val pp = pivotPosition2(h)
        val hn = if (forward) {
          val sign = if (remainingIndexes.indexOf(targetRow).ensuring(_ != -1) % 2 == 0) field.one else field.negate(field.one)
          h map { x => field.multiply(x, sign) }
        } else {
          pp match {
            case Some(p) => h map { x => field.quotient(x, h(p)) }
            case None => h
          }
        }

        val others = pp match {
          case Some(k) => {
            rest.map({
              case (row, index) =>
                (if (row(k) == field.zero) {
                  if (forward) {
                    row.drop(k + 1)
                  } else {
                    row
                  }
                } else {
                  val x = field.negate(field.quotient(row(k), h(k)))
                  val difference = (for ((hx, rx) <- (h zip row).drop(k + 1)) yield {
                    field.add(rx, field.multiply(x, hx))
                  })
                  if (forward) {
                    difference
                  } else {
                    row.take(k) ++ (field.zero +: difference)
                  }
                }, index)
            })
          }
          case None => rest
        }

        recurse(hn :: finishedRows, others, remainingIndexes.filter(_ != targetRow))
      }

    }

    import net.tqft.toolkit.collections.Pad._
    val rowsWithIndexes = rows.zipWithIndex
    val resultList = recurse(Nil, rowsWithIndexes, (0 until numberOfRows).toList).map(_.padLeft(numberOfColumns, field.zero))

    // TODO this is not so cool:
    // need this from WiredCollection
    //    val bf = rows.newBuilder
    val bf = rows.genericBuilder[Seq[B]]
    bf ++= resultList
    bf.result
  }

  def rowEchelonForm(implicit field: Field[B]): Matrix[B] = {
    Matrix(numberOfColumns, _rowReduce(entries, true))
  }

  def reducedRowEchelonForm(implicit field: Field[B]): Matrix[B] = {
    Matrix(numberOfColumns, _rowReduce(rowEchelonForm.entries.reverse, false).reverse)
  }

  def diagonals: Seq[B] = {
    for (i <- 0 until scala.math.min(numberOfRows, numberOfColumns)) yield entries(i)(i)
  }

  def preimageOf(vector: Seq[B])(implicit field: Field[B]): Option[Seq[B]] = {
    val augmentedMatrix = joinRows(Matrix.singleColumn(vector))
    val rre = augmentedMatrix.reducedRowEchelonForm

    val pivots = (rre.entries.zipWithIndex map { case (row, i) => (i, pivotPosition2(row), row.last) }).toList

    if (pivots.find(_._2 == Some(numberOfColumns)).nonEmpty) {
      None
    } else {
      val result = for (j <- (0 until numberOfColumns).toList) yield {
        pivots.find(_._2 == Some(j)) match {
          case Some((_, _, l)) => l
          case None => field.zero
        }
      }
      Some(result)
    }
  }

  // TODO move down to SquareMatrix?
  def inverse(implicit field: Field[B]): Option[Matrix[B]] = {
    if (numberOfRows != numberOfColumns) {
      None
    } else {
      val augmentedMatrix = joinRows(Matrix.identityMatrix(numberOfRows))
      val rre = augmentedMatrix.reducedRowEchelonForm

      def unitVector(k: Int) = List.fill(k)(field.zero) ::: List(field.one) ::: List.fill(numberOfRows - k - 1)(field.zero)

      rre.entries.zipWithIndex.find { case (row, k) => row.take(numberOfRows) != unitVector(k) } match {
        case None => {
          Some(Matrix(numberOfColumns, rre.entries map { row => row.drop(numberOfColumns) }))
        }
        case Some(_) => None
      }
    }
  }

  def determinant(implicit field: Field[B]): B = {
    require(numberOfRows == numberOfColumns)
    field.product(rowEchelonForm.diagonals)
  }

  def characteristicPolynomial(implicit field: Field[B]): Polynomial[B] = {
    require(numberOfRows == numberOfColumns)

    import net.tqft.toolkit.algebra.polynomials._

    val polynomials = implicitly[Polynomials[B]]
    val rationalFunctions = implicitly[Field[Fraction[Polynomial[B]]]]

    val functionMatrices = Matrices.matricesOver(numberOfRows)(rationalFunctions)

    val lift = this.mapEntries[RationalFunction[B]](x => x)
    val lambda = functionMatrices.scalarMultiply(polynomials.identity, functionMatrices.one)
    val determinant = functionMatrices.add(functionMatrices.negate(lift), lambda).determinant // it's important that lift is the first argument of add, to preserve its underlying implementation
    require(determinant.denominator == (field.one: Polynomial[B]))
    determinant.numerator
  }

  def eigenvalues(implicit field: Field[B] with Finite[B]): Set[B] = {
    import net.tqft.toolkit.algebra.polynomials._

    val solver = FiniteFieldPolynomialSolver[B]()
    import solver._
    characteristicPolynomial.roots.keySet
  }

  def eigenspace(lambda: B)(implicit field: Field[B]): Seq[Seq[B]] = {
    val matrices = Matrices.matricesOver(numberOfRows)(field)
    matrices.subtract(this, matrices.scalarMultiply(lambda, matrices.one)).nullSpace
  }

  // returns a basis for the nullspace
  def nullSpace(implicit field: Field[B]): Seq[Seq[B]] = {
    val reduced = reducedRowEchelonForm.entries

    val pivots = reduced.zipWithIndex map { case (row, i) => (i, pivotPosition2(row)) } collect { case (i, Some(j)) => (i, j) }
    val pivotColumns = pivots.map(_._2).toList
    val generators = (0 until numberOfColumns).toList filterNot (pivotColumns.contains)

    val entries = for (j <- (0 until numberOfColumns).toList) yield {
      for (g <- generators) yield {
        if (j == g) {
          field.one
        } else {
          pivots.find(_._2 == j) match {
            case Some((i, j)) => {
              field.negate(field.quotient(reduced(i)(g), reduced(i)(j)))
            }
            case None => {
              field.zero
            }
          }

        }
      }
    }

    entries.transpose
  }

  def pivotPosition(row: Int, ignoring: B) = {
    entries(row).indexWhere { b: B => b != ignoring } match {
      case -1 => None
      case k => Some(k)
    }
  }

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

  def findBasisForColumnSpace(rankBound: Option[Int] = None)(implicit field: Field[B]) = transpose.findBasisForRowSpace(rankBound)

  def par = new Matrix(numberOfColumns, entries.par)
  def seq = new Matrix(numberOfColumns, entries.seq)
}
