package net.tqft.toolkit.algebra.matrices

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.categories._
import scala.collection.immutable.SortedMap
import scala.collection.GenSeq

object Matrices extends net.tqft.toolkit.Logging {
  def matricesOver(size: Int) = new Endofunctor[Ring, Matrix] { self =>
    override def source = Rings
    override def target = Rings
    // TODO should use endomorphismAlgebra, so we can multiply by scalars from A
    override def apply[A](ring: Ring[A]): Algebra[A, Matrix[A]] = {
      implicit def r = ring
      new MatrixCategoryOverRing[A].endomorphisms(size)
    }
    override def apply[A, B](hom: Homomorphism[Ring, A, B]): RingHomomorphism[Matrix[A], Matrix[B]] = new RingHomomorphism[Matrix[A], Matrix[B]] {
      def source = self.apply(hom.source)
      def target = self.apply(hom.target)
      def apply(m: Matrix[A]) = Matrix(m.numberOfColumns, m.entries map { row => row map { hom(_) } })
    }
  }

  def tensor[B: Ring](m1: Matrix[B], m2: Matrix[B]) = new MatrixCategoryOverRing[B].tensorMorphisms(m1, m2)

  def over[A: Ring] = new MatrixCategoryOverRing[A]
  def over[A: Rig] = new MatrixCategoryOverRig[A]

}

class DenseCategoricalMatrix[A, B](sources: Seq[A], targets: Seq[A], entries: GenSeq[Seq[B]]) extends AbstractDenseCategoricalMatrix[A, B, DenseCategoricalMatrix[A, B]](sources, targets, entries)

class AbstractSparseCategoricalMatrix[A, B, M <: AbstractSparseCategoricalMatrix[A, B, M]](sources: List[A], targets: List[A], val sparseEntries: List[SortedMap[Int, B]], default: B) extends CategoricalMatrix[A, B, M](sources, targets) {
  override def entries = for (row <- sparseEntries) yield for (i <- (0 until numberOfColumns).toList) yield row.get(i).getOrElse(default)
  override def lookupEntry(row: Int)(column: Int) = sparseEntries(row).get(column).getOrElse(default)
}

class MatrixCategoryOverRig[R: Rig] extends NLinearCategory[Int, Matrix[R]] {
  val ring = implicitly[Rig[R]]

  override def identityMorphism(o: Int): Matrix[R] = Matrix.tabulate(o, o)({ (i, j) =>
    if (i == j) {
      ring.one
    } else {
      ring.zero
    }
  })
  override def zeroMorphism(o1: Int, o2: Int) = {
    val zeroRow = IndexedSeq.fill(o2)(ring.zero)
    Matrix(o1, Seq.fill(o1)(zeroRow))
  }
  override def add(x: Matrix[R], y: Matrix[R]) = Matrix.tabulate(x.numberOfRows, x.numberOfColumns)({ (i, j) =>
    ring.add(x.entries(i)(j), y.entries(i)(j))
  })
  override def compose(x: Matrix[R], y: Matrix[R]) = {
    Matrix.tabulate(x.numberOfRows, y.numberOfColumns)({ (i, j) =>
      ring.sum(for (k <- 0 until x.numberOfColumns) yield ring.multiply(x.entries(i)(k), y.entries(k)(j)))
    })
  }
  override def source(x: Matrix[R]) = x.numberOfColumns
  override def target(x: Matrix[R]) = x.numberOfRows

  //  override def endomorphismRing(o: Int) = ???
}

class MatrixCategoryOverRing[R: Ring] extends MatrixCategoryOverRig[R] with TensorCategory[Int, Matrix[R], R] {
  override val ring = implicitly[Ring[R]]
  override def negate(m: Matrix[R]) = Matrix.tabulate(m.numberOfRows, m.numberOfColumns)({ (i, j) =>
    ring.negate(m.entries(i)(j))
  })

  // there's no particular reasons these couldn't be higher up;
  // TODO cleanup the hierarchy, and allow tensor categories without negatives!
  override def scalarMultiply(a: R, m: Matrix[R]) = Matrix.tabulate(m.numberOfRows, m.numberOfColumns)({ (i, j) =>
    ring.multiply(a, m.entries(i)(j))
  })
  override def tensorObjects(o1: Int, o2: Int) = o1 * o2
  override def tensorMorphisms(m1: Matrix[R], m2: Matrix[R]) = {
    Matrix(m1.numberOfColumns * m2.numberOfColumns,
      for (row1 <- m1.entries; row2 <- m2.entries) yield VectorOperations.tensor(row1, row2))
  }

}

class MatrixCategoryOverField[F: Field] extends MatrixCategoryOverRing[F] with TensorCategory[Int, Matrix[F], F] {
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

  def compose(x: MT, y: MT): MT = {
    val entries = for (rx <- x.entries) yield {
      for (ryi <- 0 until y.numberOfColumns) yield {
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
