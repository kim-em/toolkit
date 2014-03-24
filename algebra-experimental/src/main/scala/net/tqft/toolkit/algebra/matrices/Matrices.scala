package net.tqft.toolkit.algebra.matrices

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.categories._
import scala.collection.immutable.SortedMap
import scala.collection.GenSeq
import net.tqft.toolkit.algebra.combinatorics.SumOfSquaresDecompositions

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

  lazy val positiveSymmetricDecompositionsCached: Matrix[Int] => Seq[Matrix[Int]] = {
    val bucket = net.tqft.toolkit.amazon.S3("positive-symmetric-decompositions")

    def writeMatrix(m: Matrix[Int]): String = {
      m.entries.map(_.mkString("x")).mkString("p")
    }
    def readMatrix(m: String): Matrix[Int] = {
      import net.tqft.toolkit.Extractors.Int
      m.split("p").toSeq.map(_.split("x").toSeq.collect({ case Int(n) => n }))
    }
    def writeMatrices(ms: Seq[Matrix[Int]]): String = ms.map(writeMatrix).mkString("\n")
    def readMatrices(ms: String): Seq[Matrix[Int]] = ms.split("\n").toSeq.filter(_.nonEmpty).map(readMatrix)

    import net.tqft.toolkit.collections.MapTransformer._
    val transformedBucket = bucket.transformKeys(readMatrix _, writeMatrix _).transformValues(readMatrices _, writeMatrices _)

    import net.tqft.toolkit.functions.Memo._
    ({ x: Matrix[Int] => positiveSymmetricDecompositions(x).toSeq }).memoUsing(transformedBucket)
  }
  
  // return all ways to write M=AA^t, up to permuting the columns of A
  def positiveSymmetricDecompositions(M: Matrix[Int]): Iterator[Matrix[Int]] = {
    info("finding positiveSymmetricDecompositions of " + M.entries)

    require(M.numberOfColumns == M.numberOfRows)

    def columnPermutation(A: Matrix[Int], B: Matrix[Int]): Boolean = {
      import net.tqft.toolkit.permutations.Permutations
      Permutations.mapping(A.transpose.entries.seq, B.transpose.entries.seq).nonEmpty
    }

    def partialDecompositions(m: Int): Iterator[Matrix[Int]] = {
      def newRows(d: Seq[(Int, Int)], P: Matrix[Int]): Iterator[Seq[Int]] = {
        //        println("investigating new rows for " + d + " " + P.entries)

        // TODO (minor) don't need to recompute these all the time
        val columnHashes = for(i <- 0 until P.numberOfColumns) yield P.takeColumn(i).hashCode
        val rowLengths = P.entries.map(row => row.length - row.reverse.takeWhile(_ == 0).length)

        case class PartialRow(j: Int /* == entries.size */ , reversedEntries: List[Int], gaps: Seq[Int], remaining: Map[Int, Int]) {
          //          println("entries: " + entries)
          //          println("gaps: " + gaps)
          //          println("remaining: " + remaining)
          def children: Iterator[PartialRow] = {
            for (
              next <- remaining.keysIterator;
              if ((m > 0 || next > 0) && (j == 0 || columnHashes(j) != columnHashes(j - 1) || next <= reversedEntries.head));
              newGaps = for (l <- 0 until m - 1) yield gaps(l) - next * P.entries(l)(j);
              // earlier rows end with lots of zeroes; once we reach the zeroes we should be checking that the gap is zero!
              if (for (l <- (0 until m - 1).iterator) yield if(rowLengths(l) == j + 1) { newGaps(l) == 0 }  else { newGaps(l) >= 0}).forall(_ == true);
              newRemaining = {
                if (next > 0) {
                  if (remaining(next) > 1) {
                    remaining + (next -> (remaining(next) - 1))
                  } else {
                    remaining - next
                  }
                } else {
                  remaining
                }
              }
            ) yield {
              PartialRow(j + 1, next :: reversedEntries, newGaps, newRemaining)
            }
          }
          def completions: Iterator[Seq[Int]] = {
            if (j == P.numberOfColumns) {
              // time to finish up
              if (gaps.forall(_ == 0)) {
                // TODO cleanup
                val r = reversedEntries.reverse ++ remaining.toSeq.filterNot(_._1 == 0).sortBy(_._1).flatMap(p => Seq.fill(p._2)(p._1))
                //                println("finishing up: " + r)
                Iterator(r)
              } else {
                //                println("gaps remained at the end: " + gaps)
                Iterator.empty
              }
            } else {
              children.flatMap(_.completions)
            }
          }
        }

        PartialRow(0, Nil, M.entries(m - 1).take(m - 1), d.filterNot(_._2 == 0).toMap + (0 -> 1)).completions
      }

      m match {
        case 0 => Iterator(Matrix[Int](0, Seq.empty))
        case m => {
          import net.tqft.toolkit.collections.RemoveDuplicates._
          (for (
            P <- partialDecompositions(m - 1);
            d <- SumOfSquaresDecompositions(M.entries(m - 1)(m - 1));
            v <- newRows(d, P)
          ) yield {
            val extraColumns = v.size - P.numberOfColumns;
            val zeroBlock = Matrix(extraColumns, Seq.fill(P.numberOfRows)(Seq.fill(extraColumns)(0)));
            P.joinRows(zeroBlock).appendRow(v)
          }) //.removeDuplicates(columnPermutation _)
        }
      }
    }

    val integerMatrices = new MatrixCategoryOverRing[Int]

//    // we need to filter the results; if M wasn't positive definite there are spurious answers.
//    val result = partialDecompositions(M.numberOfRows).filter(A => integerMatrices.compose(A, A.transpose) == M)
//
//    //    if (result.nonEmpty && !M.mapEntries(Conversions.integersAsDoubles).positiveSemidefinite_?) {
//    //      println("positiveSemidefinite_? seems to fail on:")
//    //      println(M)
//    //      throw new IllegalArgumentException("positiveSemidefinite_? failed on " + M)
//    //    }
//
//    result

    
    PositiveSymmetricDecomposition(M.entries.toArray.map(_.toArray)).decompositions.map(Matrix.fromArray)
  }
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
