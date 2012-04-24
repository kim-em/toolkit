package net.tqft.toolkit.algebra
import net.tqft.toolkit.Logging
import scala.collection.immutable.SortedMap
import scala.collection.GenSeq
import net.tqft.toolkit.collections.Pad

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

  def matricesOver[A](field: Field[A], size: Int): Algebra[A, Matrix[A]] = new MatrixCategoryOverField(field).endomorphismAlgebra(size)
}

abstract class CategoricalMatrix[A, B, M <: CategoricalMatrix[A, B, M]](val sources: List[A], val targets: List[A]) {
  require(targets.size == entries.size)
  //  require(entries.find(_.size != numberOfColumns).isEmpty) // not a good idea if the entries are in Hadoop

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

class AbstractDenseCategoricalMatrix[A, B, M <: AbstractDenseCategoricalMatrix[A, B, M]](sources: List[A], targets: List[A], val entries: GenSeq[Seq[B]]) extends CategoricalMatrix[A, B, M](sources, targets) {
  override def equals(other: Any) = {
    // TODO this could be optimized
    other match {
      case other: AbstractDenseCategoricalMatrix[_, _, _] => {
        sources == other.sources &&
          targets == other.targets &&
          entries.toList.size == other.entries.toList.size &&
          (for ((row1, row2) <- entries.toList zip other.entries.toList) yield {
            row1.size == row2.size && (for ((x1, x2) <- row1 zip row2) yield { x1 == x2 }).foldLeft(true)(_ && _)
          }).foldLeft(true)(_ && _)
      }
      case _ => false
    }
  }

  def lookupEntry(row: Int)(column: Int) = entries(row)(column)
  def pivotPosition(row: Int, ignoring: B) =
    entries(row).indexWhere { b: B => b != ignoring } match {
      case -1 => None
      case k => Some(k)
    }

}

class DenseCategoricalMatrix[A, B](sources: List[A], targets: List[A], entries: GenSeq[Seq[B]]) extends AbstractDenseCategoricalMatrix[A, B, DenseCategoricalMatrix[A, B]](sources, targets, entries)

class AbstractSparseCategoricalMatrix[A, B, M <: AbstractSparseCategoricalMatrix[A, B, M]](sources: List[A], targets: List[A], val sparseEntries: List[SortedMap[Int, B]], default: B) extends CategoricalMatrix[A, B, M](sources, targets) {
  def entries = for (row <- sparseEntries) yield for (i <- (0 until numberOfColumns).toList) yield row.get(i).getOrElse(default)
  def lookupEntry(row: Int)(column: Int) = sparseEntries(row).get(column).getOrElse(default)
  def pivotPosition(row: Int, ignoring: B) = (sparseEntries(row).find { _._2 != ignoring }).map(_._1)
}

object Matrix extends Logging {
  def apply[B](numberOfColumns: Int, entries: GenSeq[Seq[B]]): Matrix[B] = new Matrix(numberOfColumns, entries)
  def apply[B](entries: GenSeq[Seq[B]]): Matrix[B] = {
    require(entries.nonEmpty)
    apply(entries.head.size, entries)
  }

  // FIXME deprecate this in favour of the pivotPosition defined on CategoricalMatrix
  private def pivotPosition2[B](row: Seq[B])(implicit field: Field[B]): Option[Int] = row.indexWhere { b: B => b != field.zero } match {
    case -1 => None
    case k => Some(k)
  }

  def singleColumn[B](vector: List[B]) = new Matrix(1, vector map { x => List(x) })

  def diagonalMatrix[B](vector: List[B])(implicit field: Field[B]) = new Matrix(vector.size, vector.zipWithIndex map { case (v, k) => List.fill(k)(field.zero) ::: List(v) ::: List.fill(vector.size - k - 1)(field.zero) })

  def identityMatrix[B](size: Int)(implicit field: Field[B]) = diagonalMatrix(List.fill(size)(field.one))
}

class Matrix[B](
  override val numberOfColumns: Int,
  override val entries: GenSeq[Seq[B]]) extends AbstractDenseCategoricalMatrix[Unit, B, Matrix[B]](List.fill(numberOfColumns)(()), List.fill(entries.size)(()), entries) {

  def mapRows[A](rowMapper: Seq[B] => Seq[A], newColumnSize: Int = numberOfColumns) = new Matrix(newColumnSize, entries.map(rowMapper))
  def mapEntries[A](entryMapper: B => A) = new Matrix(numberOfColumns, entries.map(row => row.map(entryMapper)))

  override def toString = (entries.toList.map { r => r.mkString("(", ", ", ")") }).mkString("\n")

  import Matrix.pivotPosition2

  def transpose = new Matrix(numberOfRows, entries.toList.transpose)

  def takeColumn(column: Int) = entries map { row => row(column) }

  def takeColumns(columns: List[Int]) = new Matrix(columns.size, entries map { row => columns map { i => row(i) } })
  def dropColumns(columns: List[Int]) = takeColumns((0 until numberOfColumns).toList filterNot (columns contains _))

  def appendRow(row: Seq[B]) = {
    require(row.size == numberOfColumns)
    Matrix(numberOfColumns, entries :+ row)
  }

  def joinRows(other: Matrix[B]) = new Matrix(numberOfColumns + other.numberOfColumns, (entries zip other.entries) map { case (r1, r2) => r1 ++ r2 })

  def apply(vector: List[B])(implicit field: Field[B]) = {
    for (row <- entries) yield {
      import AlgebraicNotation._
      sum(for ((x, y) <- row zip vector) yield field.multiply(x, y))(field)
    }
  }

  private def chop(x: B)(implicit field: Field[B]) = {
    field match {
      case field: ApproximateField[_] => field.chop(x)
      case _ => x
    }
  }

  //  trait CollectionProxy[A] {
  //    def filter(f: A => Boolean): CollectionProxy[A]
  //    def toList: List[A]
  //    def map[B <: Serializable](f: A => B)(implicit manifest: Manifest[B]): CollectionProxy[B]
  //  }
  //
  //  class DListCollectionProxy[A <: Serializable](dList: com.nicta.scoobi.DList[A]) extends CollectionProxy[A] {
  //    def this(seq: Seq[A])(implicit manifest: Manifest[A]) = this(com.nicta.scoobi.DList(seq: _*))
  //    import net.tqft.toolkit.hadoop.ScoobiHelper._
  //
  //    def filter(f: A => Boolean): CollectionProxy[A] = new DListCollectionProxy(dList.filter(f))
  //    def toList: List[A] = dList.hither.toList
  //    def map[B <: Serializable](f: A => B)(implicit manifest: Manifest[B]): CollectionProxy[B] = {
  //      import com.nicta.scoobi.WireFormat._
  //      new DListCollectionProxy(dList.map(f))
  //    }
  //  }
  //
  //  class GenSeqCollectionProxy[A](seq: GenSeq[A]) extends CollectionProxy[A] {
  //    def filter(f: A => Boolean): CollectionProxy[A] = new GenSeqCollectionProxy(seq.filter(f))
  //    def toList: List[A] = seq.toList
  //    def map[B <: Serializable](f: A => B)(implicit manifest: Manifest[B]): CollectionProxy[B] = {
  //      new GenSeqCollectionProxy(seq.map(f))
  //    }
  //  }

  private def rowPriority(row: Seq[B])(implicit field: Field[B]) = {
    field match {
      case o: OrderedField[_] => {
        implicit val orderedField = o
        pivotPosition2(row)(field).map(i => (i, field.negate(orderedField.abs(row(i))))).getOrElse((Integer.MAX_VALUE, field.zero))
      }
      case _ => (pivotPosition2(row).getOrElse(Integer.MAX_VALUE), field.zero)
    }
  }

  private def elementOrdering(implicit field: Field[B]) = {
    field match {
      case field: OrderedField[_] => field
      case _ => new Ordering[B] { def compare(x: B, y: B) = 0 }
    }
  }
  
  // TODO just use sortBy, and get rid of this?
  // but GenSeq doesn't have sortBy, lame.
  private def selectTargetRow(rows: GenSeq[(Seq[B], Int)], rowIndexes: Seq[Int], forward: Boolean)(implicit field: Field[B]) = {
    implicit val ordering = elementOrdering
    if (forward) {
      rows.map({ case (row, index) => (rowPriority(row), index) }).toList.sortBy(_._1).head._2
    } else {
      rowIndexes.head
    }
  }

  private def fieldElementManifest(implicit field: Field[B]): Manifest[B] = ClassManifest.singleType(field.zero.asInstanceOf[B with AnyRef]).asInstanceOf[Manifest[B]]

  private def _rowReduce(rows: GenSeq[Seq[B]], forward: Boolean)(implicit field: Field[B]): GenSeq[Seq[B]] = {
    // we carry around the row indexes separately, because remainingRows might be living off in Hadoop or something...

    implicit val bManifest = fieldElementManifest

    @scala.annotation.tailrec
    def recurse(finishedRows: List[Seq[B]], remainingRows: GenSeq[(Seq[B], Int)], remainingIndexes: Seq[Int]): List[Seq[B]] = {
      println("finished " + finishedRows.size + " rows")

      if (remainingIndexes.isEmpty) {
        finishedRows.reverse
      } else {
        val targetRow = selectTargetRow(remainingRows, remainingIndexes, forward)
        
        val h = remainingRows.filter(_._2 == targetRow).toList.head._1
        val rest = remainingRows.filter(_._2 != targetRow)
        val pp = pivotPosition2(h)
        val hn = if (forward) {
          h
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

    import Pad._
    val rowsWithIndexes = rows.zipWithIndex
    val resultList = recurse(Nil, rowsWithIndexes, 0 until numberOfRows toList).map(_.padLeft(numberOfColumns, field.zero))

    // TODO this is not so cool:
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

  def preimageOf(vector: List[B])(implicit field: Field[B]): Option[List[B]] = {
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

    val pivots = reduced.zipWithIndex map { case (row, i) => (i, pivotPosition2(row)) } collect { case (i, Some(j)) => (i, j) }
    val pivotColumns = pivots.map(_._2).toList
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

  def findBasisForColumnSpace(rankBound: Option[Int] = None)(implicit field: Field[B]) = transpose.findBasisForRowSpace(rankBound)
}

class MatrixCategoryOverRing[R](ring: Ring[R]) extends LinearCategory[Int, Matrix[R], R] {
  val inner = new AbstractMatrixCategory(ring)({ (sources: List[Unit], targets: List[Unit], entries: GenSeq[Seq[R]]) => Matrix(sources.size, entries) })

  override def identityMorphism(o: Int) = inner.identityMorphism(List.fill(o)(()))
  override def zeroMorphism(o1: Int, o2: Int) = inner.zeroMorphism(List.fill(o1)(()), List.fill(o2)(()))
  override def negate(m: Matrix[R]) = inner.negate(m)
  override def add(x: Matrix[R], y: Matrix[R]) = inner.add(x, y)
  override def compose(x: Matrix[R], y: Matrix[R]) = inner.compose(x, y)
  override def source(x: Matrix[R]) = x.numberOfColumns
  override def target(x: Matrix[R]) = x.numberOfRows
  override def scalarMultiply(a: R, m: Matrix[R]) = inner.buildMatrix(m.sources, m.targets, m.entries map { r => r map { x => ring.multiply(a, x) } })

}

class MatrixCategoryOverField[F](field: Field[F]) extends MatrixCategoryOverRing(field) with LinearCategory[Int, Matrix[F], F] {
  /* FIXME override */ def inverseOption(x: Matrix[F]) = None // TODO
}

class MatrixCategory[O, M](entryCategory: AdditiveCategory[O, M]) extends AbstractMatrixCategory[O, M, DenseCategoricalMatrix[O, M]](entryCategory)(new DenseCategoricalMatrix(_, _, _))

class AbstractMatrixCategory[O, M, MT <: CategoricalMatrix[O, M, MT]](
  entryCategory: AdditiveCategory[O, M])(
    implicit val buildMatrix: (List[O], List[O], GenSeq[Seq[M]]) => MT) extends AdditiveCategory[List[O], MT] {

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
