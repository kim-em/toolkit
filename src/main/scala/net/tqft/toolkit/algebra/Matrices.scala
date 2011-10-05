package net.tqft.toolkit.algebra
import net.tqft.toolkit.Logging

object Matrices {
  def matricesOver(size: Int) = new Endofunctor[Ring, Matrix] { self =>
    def source = Rings
    def target = Rings
    def apply[A](ring: Ring[A]): Ring[Matrix[A]] = new MatrixCategoryOverRing(ring).endomorphismRing(size)
    def apply[A, B](hom: Homomorphism[Ring, A, B]): RingHomomorphism[Matrix[A], Matrix[B]] = new RingHomomorphism[Matrix[A], Matrix[B]] {
      def source = self.apply(hom.source)
      def target = self.apply(hom.target)
      def apply(m: Matrix[A]) = Matrix(m.numberOfColumns, m.entries map { row => row map { hom(_) } })
    }
  }

  // testing!
  val intMatrix = Matrix(3, List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
  val rationalMatrix = matricesOver(3)(Gadgets.integersAsRationals)(intMatrix)
  val ns = rationalMatrix.nullSpace(Gadgets.Rationals)

  def matricesOver[A](field: Field[A], size: Int): Ring[Matrix[A]] = new MatrixCategoryOverField(field).endomorphismRing(size)
}

case class CategoricalMatrix[A, B](sources: List[A], targets: List[A], entries: List[List[B]]) {
  require(targets.size == entries.size)
  require(entries.find(_.size != numberOfColumns).isEmpty)

  def numberOfColumns = sources.size
  def numberOfRows = entries.size
}

object Matrix extends Logging {
  def apply[B](numberOfColumns: Int, entries: List[List[B]]): Matrix[B] = new Matrix(numberOfColumns, entries)
  def apply[B](entries: List[List[B]]): Matrix[B] = {
    require(entries.nonEmpty)
    apply(entries.head.size, entries)
  }

  private def pivotPosition[B](row: List[B])(implicit field: Field[B]): Option[Int] = row.indexWhere { b: B => b != field.zero } match {
    case -1 => None
    case k => Some(k)
  }

  def findBasisForRowSpace[B](entries: Iterable[List[B]], rankBound: Option[Int] = None)(implicit field: Field[B]): List[Int] = {
    case class Progress(rowsConsidered: Int, basis: List[Int], reducedRows: Option[Matrix[B]])

    def considerAnotherRow(soFar: Progress, row: List[B]): Progress = {
      if (rankBound == Some(soFar.basis.size)) {
        // we've found enough, short circuit
        soFar
      } else {
        val next = (soFar.reducedRows match {
          case None => Matrix(row.size, List(row))
          case Some(m) => m.appendRow(row)
        }).rowEchelonForm

        next.entries.lastOption match {
          case Some(lastRow) if pivotPosition(lastRow).nonEmpty => {
            Progress(soFar.rowsConsidered + 1, soFar.rowsConsidered :: soFar.basis, Some(next))
          }
          case _ /* Some(zeroRow) or None */ => {
            Progress(soFar.rowsConsidered + 1, soFar.basis, soFar.reducedRows)
          }
        }
      }
    }

    entries.foldLeft(Progress(0, List(), None))(considerAnotherRow _).basis.reverse
  }

  def singleColumn[B](vector: List[B]) = new Matrix(1, vector map { x => List(x) })

  def diagonalMatrix[B](vector: List[B])(implicit field: Field[B]) = new Matrix(vector.size, vector.zipWithIndex map { case (v, k) => List.fill(k)(field.zero) ::: List(v) ::: List.fill(vector.size - k - 1)(field.zero) })

  def identityMatrix[B](size: Int)(implicit field: Field[B]) = diagonalMatrix(List.fill(size)(field.one))
}

class Matrix[B](
  override val numberOfColumns: Int,
  override val entries: List[List[B]]) extends CategoricalMatrix[Unit, B](List.fill(numberOfColumns)(()), List.fill(entries.size)(()), entries) {

  import Matrix.pivotPosition

  def transpose = new Matrix(numberOfRows, entries.transpose)

  def takeColumn(column: Int) = entries map { row => row(column) }

  def takeColumns(columns: List[Int]) = new Matrix(columns.size, entries map { row => columns map { i => row(i) } })
  def dropColumns(columns: List[Int]) = takeColumns((0 until numberOfColumns).toList filterNot (columns contains _))

  def appendRow(row: List[B]) = {
    require(row.size == numberOfColumns)
    Matrix(numberOfColumns, entries ::: List(row))
  }

  def joinRows(other: Matrix[B]) = new Matrix(numberOfColumns + other.numberOfColumns, (entries zip other.entries) map { case (r1, r2) => r1 ::: r2 })

  def apply(vector: List[B])(implicit field: Field[B]) = {
    for (row <- entries) yield {
      import AlgebraicNotation._
      sum(for ((x, y) <- row zip vector) yield field.multiply(x, y))(field)
    }
  }

  private def _rowReduce(rows: List[List[B]], sort: Boolean)(implicit field: Field[B]): List[List[B]] = {
    val sortedRows = if (sort) {
      rows.sortBy { pivotPosition(_).getOrElse(Integer.MAX_VALUE) }
    } else {
      rows
    }
    sortedRows match {
      case Nil => Nil
      case h :: rest => {
        val pp = pivotPosition(h)

        val hn = pp match {
          case Some(p) => h map { x => field.quotient(x, h(p)) }
          case None => h
        }

        val others = pp match {
          case Some(k) => {
            rest map { row =>
              if (row(k) == field.zero) {
                row
              } else {
                val x = field.negate(field.quotient(row(k), h(k)))
                for ((hx, rx) <- h zip row) yield {
                  field.add(rx, field.multiply(x, hx))
                }
              }
            }
          }
          case None => rest
        }

        hn :: _rowReduce(others, sort)
      }
    }
  }

  def rowEchelonForm(implicit field: Field[B]): Matrix[B] = {
    Matrix(numberOfColumns, _rowReduce(entries, true))
  }

  def reducedRowEchelonForm(implicit field: Field[B]): Matrix[B] = {
    Matrix(numberOfColumns, _rowReduce(rowEchelonForm.entries.reverse, false).reverse)
  }

  def preimageOf(vector: List[B])(implicit field: Field[B]): Option[List[B]] = {
    require(numberOfRows == vector.size)

    val augmentedMatrix = joinRows(Matrix.singleColumn(vector))
    val rre = augmentedMatrix.reducedRowEchelonForm

    if (rre.entries.collect { case row if pivotPosition(row) == Some(numberOfColumns) => false } nonEmpty) {
      None
    } else {
      val pivots = rre.entries.zipWithIndex map { case (row, i) => (i, pivotPosition(row)) } collect { case (i, Some(j)) => (i, j) }
      val pivotColumns = pivots.map(_._2)
      val result = for (j <- (0 until numberOfColumns).toList) yield {
        pivots.find(_._2 == j) match {
          case Some((i, j)) => rre.entries(i).last
          case None => field.zero
        }
      }

      assert(apply(result) == vector)

      Some(result)
    }
  }

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

  // returns a basis for the nullspace as the columns of a new matrix
  def nullSpace(implicit field: Field[B]): Matrix[B] = {
    val reduced = reducedRowEchelonForm.entries

    val pivots = reduced.zipWithIndex map { case (row, i) => (i, pivotPosition(row)) } collect { case (i, Some(j)) => (i, j) }
    val pivotColumns = pivots.map(_._2)
    val generators = (0 until numberOfColumns).toList filterNot (pivotColumns contains)

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

    Matrix(generators.size, entries)
  }

  def findBasisForRowSpace(implicit field: Field[B]) = Matrix.findBasisForRowSpace(entries)
  def findBasisForColumnSpace(implicit field: Field[B]) = Matrix.findBasisForRowSpace(transpose.entries)

  def rank(implicit field: Field[B]): Int = findBasisForRowSpace.size
}

class MatrixCategoryOverRing[R](ring: Ring[R]) extends LinearCategory[Int, Matrix[R]] {
  val inner = new AbstractMatrixCategory(ring)({ (sources: List[Unit], targets: List[Unit], entries: List[List[R]]) => Matrix(sources.size, entries) })

  def identityMorphism(o: Int) = inner.identityMorphism(List.fill(o)(()))
  def zeroMorphism(o1: Int, o2: Int) = inner.zeroMorphism(List.fill(o1)(()), List.fill(o2)(()))
  def negate(m: Matrix[R]) = inner.negate(m)
  def add(x: Matrix[R], y: Matrix[R]) = inner.add(x, y)
  def compose(x: Matrix[R], y: Matrix[R]) = inner.compose(x, y)
  def source(x: Matrix[R]) = x.numberOfColumns
  def target(x: Matrix[R]) = x.numberOfRows
}

class MatrixCategoryOverField[F](field: Field[F]) extends MatrixCategoryOverRing(field) with LinearCategory[Int, Matrix[F]] {
  def inverseOption(x: Matrix[F]) = None // TODO
}

class MatrixCategory[O, M](entryCategory: LinearCategory[O, M]) extends AbstractMatrixCategory[O, M, CategoricalMatrix[O, M]](entryCategory)(CategoricalMatrix.apply _)

class AbstractMatrixCategory[O, M, MT <: CategoricalMatrix[O, M]](
  entryCategory: LinearCategory[O, M])(
    implicit buildMatrix: (List[O], List[O], List[List[M]]) => MT) extends LinearCategory[List[O], MT] {

  def identityMorphism(o: List[O]) = {

    val entries = for ((o1, k1) <- o zipWithIndex) yield {
      for ((o2, k2) <- o zipWithIndex) yield {
        if (k1 == k2) {
          entryCategory.identityMorphism(o1)
        } else {
          entryCategory.zeroMorphism(o1, o2)
        }
      }
    }

    buildMatrix(o, o, entries)
  }
  def zeroMorphism(o1: List[O], o2: List[O]) = {

    val entries = for (oo2 <- o2) yield {
      for (oo1 <- o1) yield {
        entryCategory.zeroMorphism(oo1, oo2)
      }
    }

    buildMatrix(o1, o2, entries)
  }
  def negate(m: MT) = {
    buildMatrix(m.sources, m.targets, m.entries map { r => r map { x => entryCategory.negate(x) } })
  }
  def add(x: MT, y: MT) = {
    require(x.sources == y.sources)
    require(x.targets == y.targets)

    val entries = (x.entries zip y.entries) map { case (rx, ry) => (rx zip ry) map { case (ex, ey) => entryCategory.add(ex, ey) } }
    buildMatrix(x.sources, y.targets, entries)
  }

  def compose(x: MT, y: MT) = {
    val entries = for (rx <- x.entries) yield {
      for (ryi <- (0 until y.numberOfColumns).toList) yield {
        (rx zip (y.entries map { _(ryi) })) map { case (ex, ey) => entryCategory.compose(ex, ey) } reduceLeft { entryCategory.add(_, _) }
      }
    }

    buildMatrix(y.sources, x.targets, entries)
  }

  def target(m: MT) = {
    m.targets
  }
  def source(m: MT) = {
    m.sources
  }

}
