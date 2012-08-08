package net.tqft.toolkit.algebra
import net.tqft.toolkit.Logging
import scala.collection.immutable.SortedMap
import scala.collection.GenSeq
import net.tqft.toolkit.collections.Pad

object Matrices {
  def matricesOver(size: Int) = new Endofunctor[Ring, Matrix] { self =>
    def source = Rings
    def target = Rings
    // TODO should use endomorphismAlgebra, so we can multiply by scalars from A
    def apply[A](ring: Ring[A]): Algebra[A, Matrix[A]] = new MatrixCategoryOverRing(ring).endomorphismAlgebra(size)
    def apply[A, B](hom: Homomorphism[Ring, A, B]): RingHomomorphism[Matrix[A], Matrix[B]] = new RingHomomorphism[Matrix[A], Matrix[B]] {
      def source = self.apply(hom.source)
      def target = self.apply(hom.target)
      def apply(m: Matrix[A]) = Matrix(m.numberOfColumns, m.entries map { row => row map { hom(_) } })
    }
  }

  def tensor[B: Ring](m1: Matrix[B], m2: Matrix[B]) = new MatrixCategoryOverRing(implicitly[Ring[B]]).tensorMorphisms(m1, m2)

  def over[A](ring: Ring[A], size: Int): Algebra[A, Matrix[A]] = new MatrixCategoryOverRing(ring).endomorphismAlgebra(size)
  //  def matricesOver[A](field: Field[A], size: Int): Algebra[A, Matrix[A]] = new MatrixCategoryOverField(field).endomorphismAlgebra(size)

  // return all ways to write M=AA^t, up to permuting the columns of A
  def positiveSymmetricDecompositions(M: Matrix[Int]): Iterator[Matrix[Int]] = {
    require(M.numberOfColumns == M.numberOfRows)

    def columnPermutation(A: Matrix[Int], B: Matrix[Int]): Boolean = {
      import net.tqft.toolkit.permutations.Permutations
      Permutations.mapping(A.transpose.entries.seq, B.transpose.entries.seq).nonEmpty
    }

    def partialDecompositions(m: Int): Iterator[Matrix[Int]] = {
      import Gadgets.Integers

      def newRows(d: Seq[(Int, Int)], P: Matrix[Int]): Iterator[Seq[Int]] = {
        //        println("investigating new rows for " + d + " " + P.entries)

        def candidates(j: Int, remaining: Seq[Int], gaps: Seq[Int]): Iterator[Seq[Int]] = {
          //          println(List.fill(m-j)(" ").mkString("") + "running candidates(j = " + j + ", remaining = " + remaining + ", gaps = " + gaps + ")")
          require(gaps.size == m - 1)

          j match {
            case 0 => {
              if (gaps.forall(_ == 0)) {
                Iterator(Seq.empty)
              } else {
                Iterator.empty
              }
            }
            case j => {
              for (
                next <- (0 +: remaining.distinct).iterator;
                newGaps = {
                  for (l <- 0 until m - 1) yield gaps(l) - next * P.entries(l)(j - 1)
                };
                if (newGaps.forall(_ >= 0));
                newRemaining = {
                  import net.tqft.toolkit.collections.DeleteOne._;
                  remaining.deleteAtMostOne(next)
                };
                c <- candidates(j - 1, newRemaining, newGaps)
              ) yield c :+ next
            }
          }
        }

        val d_expanded = d.flatMap(p => Seq.fill(p._2)(p._1))
        for (candidate <- candidates(P.numberOfColumns, d_expanded, M.entries(m - 1).take(m - 1))) yield {
          //          println(candidate)
          candidate ++ (d_expanded diff candidate)
        }
      }

      m match {
        case 0 => Iterator(Matrix[Int](0, Seq.empty))
        case m => {
          import net.tqft.toolkit.collections.RemoveDuplicates._
          (for (
            P <- partialDecompositions(m - 1);
            d <- Integers.sumOfSquaresDecomposition(M.entries(m - 1)(m - 1));
            v <- newRows(d, P)
          ) yield {
            val extraColumns = v.size - P.numberOfColumns;
            val zeroBlock = Matrix(extraColumns, Seq.fill(P.numberOfRows)(Seq.fill(extraColumns)(0)));
            P.joinRows(zeroBlock).appendRow(v)
          }).removeDuplicates(columnPermutation _)
        }
      }
    }

    val integerMatrices = new MatrixCategoryOverRing(Gadgets.Integers)

    // we need to filter the results; if M wasn't positive definite there are spurious answers.
    partialDecompositions(M.numberOfRows).filter(A => integerMatrices.compose(A, A.transpose) == M)
    
  }
}

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

class AbstractDenseCategoricalMatrix[A, B, M <: AbstractDenseCategoricalMatrix[A, B, M]](sources: Seq[A], targets: Seq[A], val entries: GenSeq[Seq[B]]) extends CategoricalMatrix[A, B, M](sources, targets) {
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

class DenseCategoricalMatrix[A, B](sources: Seq[A], targets: Seq[A], entries: GenSeq[Seq[B]]) extends AbstractDenseCategoricalMatrix[A, B, DenseCategoricalMatrix[A, B]](sources, targets, entries)

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

  def singleColumn[B](vector: Seq[B]) = new Matrix(1, vector map { x => List(x) })

  def diagonalMatrix[B](vector: Seq[B])(implicit field: Field[B]) = new Matrix(vector.size, vector.zipWithIndex map { case (v, k) => List.fill(k)(field.zero) ::: List(v) ::: List.fill(vector.size - k - 1)(field.zero) })

  def identityMatrix[B](size: Int)(implicit field: Field[B]) = diagonalMatrix(List.fill(size)(field.one))
}

class Matrix[B](
  override val numberOfColumns: Int,
  override val entries: GenSeq[Seq[B]]) extends AbstractDenseCategoricalMatrix[Unit, B, Matrix[B]](List.fill(numberOfColumns)(()), List.fill(entries.size)(()), entries) {

  def mapRows[C](rowMapper: Seq[B] => Seq[C], newColumnSize: Int = numberOfColumns) = new Matrix(newColumnSize, entries.map(rowMapper))
  def mapEntries[C](entryMapper: B => C) = new Matrix(numberOfColumns, entries.map(row => row.map(entryMapper)))

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

  def joinRows(other: Matrix[B]) = {
    new Matrix(numberOfColumns + other.numberOfColumns, (entries zip other.entries) map { case (r1, r2) => r1 ++ r2 })
  }

  def apply(vector: Seq[B])(implicit rig: Rig[B]) = {
    (for (row <- entries) yield {
      rig.add(for ((x, y) <- row zip vector) yield rig.multiply(x, y))
    }).seq
  }

  def trace(implicit addition: CommutativeMonoid[B]): B = {
    require(numberOfRows == numberOfColumns)
    addition.add(for ((row, i) <- entries.zipWithIndex) yield row(i))
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
    field.multiply(rowEchelonForm.entries.zipWithIndex.map({ case (r, i) => r(i) }).seq)
  }

  def characteristicPolynomial(implicit field: Field[B]): Polynomial[B] = {
    require(numberOfRows == numberOfColumns)

    implicit val rationalFunctions = RationalFunctions.over(field)
    val functionMatrices = Matrices.matricesOver(numberOfRows)(rationalFunctions)
    val lift = Matrices.matricesOver(numberOfRows)(RationalFunctions.embeddingAsConstants(field))(this)
    val lambda = functionMatrices.scalarMultiply(RationalFunction.identity, functionMatrices.one)
    val determinant = functionMatrices.add(functionMatrices.negate(lift), lambda).determinant // it's important that lift is the first argument of add, to preserve its underlying implementation
    require(determinant.denominator == Polynomials.embeddingAsConstants(field)(field.one))
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

class MatrixCategoryOverRing[R](ring: Ring[R]) extends TensorCategory[Int, Matrix[R], R] {
  val inner = new AbstractMatrixCategory(ring)({ (sources: Seq[Unit], targets: Seq[Unit], entries: GenSeq[Seq[R]]) => Matrix(sources.size, entries) }) {
    override def add(x: Matrix[R], y: Matrix[R]) = {
      require(x.numberOfColumns == y.numberOfColumns)
      require(x.numberOfRows == y.numberOfRows)

      val entries = (x.entries zip y.entries) map { case (rx, ry) => VectorOperations.add(rx, ry)(ring) }
      buildMatrix(x.sources, y.targets, entries)
    }

  }

  override def identityMorphism(o: Int) = inner.identityMorphism(List.fill(o)(()))
  override def zeroMorphism(o1: Int, o2: Int) = {
    val zero = ring.zero
    val zeroRow = Seq.fill(o2)(zero)
    new Matrix(o1, Seq.fill(o1)(zeroRow))
  }
  override def negate(m: Matrix[R]) = inner.negate(m)
  override def add(x: Matrix[R], y: Matrix[R]) = inner.add(x, y)
  override def compose(x: Matrix[R], y: Matrix[R]) = inner.compose(x, y)
  override def source(x: Matrix[R]) = x.numberOfColumns
  override def target(x: Matrix[R]) = x.numberOfRows
  override def scalarMultiply(a: R, m: Matrix[R]) = inner.buildMatrix(m.sources, m.targets, m.entries map { r => VectorOperations.scalarMultiply(a, r)(ring) })
  override def tensorObjects(o1: Int, o2: Int) = o1 * o2
  override def tensorMorphisms(m1: Matrix[R], m2: Matrix[R]) = {
    new Matrix(m1.numberOfColumns * m2.numberOfColumns,
      for (row1 <- m1.entries; row2 <- m2.entries) yield VectorOperations.tensor(row1, row2)(ring))
  }

  //  override def endomorphismRing(o: Int) = ???
}

class MatrixCategoryOverField[F](field: Field[F]) extends MatrixCategoryOverRing(field) with TensorCategory[Int, Matrix[F], F] {
  /* FIXME override */ def inverseOption(x: Matrix[F]) = None // TODO
}

class MatrixCategory[O, M](entryCategory: AdditiveCategory[O, M]) extends AbstractMatrixCategory[O, M, DenseCategoricalMatrix[O, M]](entryCategory)(new DenseCategoricalMatrix(_, _, _))

class AbstractMatrixCategory[O, M, MT <: CategoricalMatrix[O, M, MT]](
  entryCategory: AdditiveCategory[O, M])(
    implicit val buildMatrix: (Seq[O], Seq[O], GenSeq[Seq[M]]) => MT) extends AdditiveCategory[Seq[O], MT] {

  def identityMorphism(o: Seq[O]) = {

    val entries = for ((o1, k1) <- o.zipWithIndex) yield {
      for ((o2, k2) <- o.zipWithIndex) yield {
        if (k1 == k2) {
          entryCategory.identityMorphism(o1)
        } else {
          entryCategory.zeroMorphism(o1, o2)
        }
      }
    }

    buildMatrix(o, o, entries)
  }
  def zeroMorphism(o1: Seq[O], o2: Seq[O]) = {

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
