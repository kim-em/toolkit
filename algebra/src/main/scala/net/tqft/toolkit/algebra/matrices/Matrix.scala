package net.tqft.toolkit.algebra.matrices

import scala.collection.GenSeq
import net.tqft.toolkit.algebra._

object Matrix extends net.tqft.toolkit.Logging {
  def apply[B](numberOfColumns: Int, entries: GenSeq[Seq[B]]): Matrix[B] = new Matrix(numberOfColumns, entries.map({
    case r: IndexedSeq[B] => r
    case r => r.toIndexedSeq[B]
  }))
  implicit def from[B](entries: GenSeq[Seq[B]]): Matrix[B] = {
    require(entries.nonEmpty)
    apply(entries.head.size, entries)
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
    Matrix(numberOfColumns, Seq.tabulate(numberOfRows, numberOfColumns)(entries))
  }
}

class Matrix[B](
  override val numberOfColumns: Int,
  override val entries: GenSeq[IndexedSeq[B]]) extends AbstractDenseCategoricalMatrix[Unit, B, Matrix[B]](List.fill(numberOfColumns)(()), List.fill(entries.size)(()), entries) {

  def mapRows[C](rowMapper: IndexedSeq[B] => IndexedSeq[C], newColumnSize: Int = numberOfColumns) = new Matrix(newColumnSize, entries.map(rowMapper))
  def mapEntries[C](entryMapper: B => C) = new Matrix(numberOfColumns, entries.map(row => row.map(entryMapper)))

  override def toString = "Matrix(" + numberOfColumns + ", " + entries + ")"

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
    addition.sum(for ((row, i) <- entries.zipWithIndex) yield row(i))
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

  private def fieldElementManifest(implicit field: Field[B]): Manifest[B] = ClassManifest.singleType(field.zero.asInstanceOf[B with AnyRef]).asInstanceOf[Manifest[B]]

  private def _rowReduce(rows: GenSeq[Seq[B]], forward: Boolean)(implicit field: Field[B]): GenSeq[Seq[B]] = {
    // we carry around the row indexes separately, because remainingRows might be living off in Hadoop or something...

    implicit val bManifest = fieldElementManifest
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

  def positiveSemidefinite_?(implicit field: ApproximateReals[B]): Boolean = {
    require(numberOfRows == numberOfColumns)

    choleskyDecomposition.nonEmpty
  }

  def choleskyDecomposition(implicit field: ApproximateReals[B]): Option[Matrix[B]] = {
    require(numberOfColumns == numberOfRows)

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
          field.quotient(field.subtract(entries(i)(j), field.sum(for (k <- 0 until j) yield field.multiply(L(i, k), L(j, k)))), L(j, j))
        }
      } else {
        field.zero
      }
//      println(i + " " + j + " " + result)
      result
    })

    lazy val diagonalEntries: Int => Option[B] = Memo({ i: Int =>
      val r = field.subtract(entries(i)(i), field.sum(for (k <- 0 until i) yield field.power(L(i, k), 2)))
      val result = if (field.compare(field.chop(r), field.zero) < 0) {
        None
      } else if (field.compare(field.chop(r), field.zero) == 0) {
        Some(field.zero)
      } else {
        Some(field.sqrt(r))
      }
//      println(i + " " + result)
      result
    })

    if ((0 until numberOfColumns).forall(i => diagonalEntries(i).nonEmpty)) {
      Some(Matrix(numberOfColumns, Seq.tabulate(numberOfColumns, numberOfColumns)({ (i, j) => L(i, j) })))
    } else {
      None
    }
  }

  def characteristicPolynomial(implicit field: Field[B]): polynomials.Polynomial[B] = {
    require(numberOfRows == numberOfColumns)

    import polynomials._

    implicit val rationalFunctions = RationalFunctions.over(field)
    val functionMatrices = Matrices.matricesOver(numberOfRows)(rationalFunctions)
    val lift = Matrices.matricesOver(numberOfRows)(RationalFunctions.embeddingAsConstants(field))(this)
    val lambda = functionMatrices.scalarMultiply(RationalFunction.identity, functionMatrices.one)
    val determinant = functionMatrices.add(functionMatrices.negate(lift), lambda).determinant // it's important that lift is the first argument of add, to preserve its underlying implementation
    require(determinant.denominator == polynomials.Polynomials.embeddingAsConstants(field)(field.one))
    determinant.numerator
  }

  def eigenvalues(implicit field: Field[B] with Elements[B]): Set[B] = characteristicPolynomial.roots

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

  def findBasisForColumnSpace(rankBound: Option[Int] = None)(implicit field: Field[B]) = transpose.findBasisForRowSpace(rankBound)

  def par = new Matrix(numberOfColumns, entries.par)
  def seq = new Matrix(numberOfColumns, entries.seq)
}
