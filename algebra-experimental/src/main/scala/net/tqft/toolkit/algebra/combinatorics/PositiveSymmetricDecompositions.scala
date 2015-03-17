package net.tqft.toolkit.algebra.combinatorics

import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.Logging

object PositiveSymmetricDecompositions extends Logging {
  lazy val cached: Matrix[Int] => Seq[Matrix[Int]] = {
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
    ({ x: Matrix[Int] => this.apply(x).toSeq }).memoUsing(transformedBucket)
  }

  // return all ways to write M=AA^t, up to permuting the columns of A
  def apply(M: Matrix[Int]): Iterator[Matrix[Int]] = {
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
        val columnHashes = for (i <- 0 until P.numberOfColumns) yield P.takeColumn(i).hashCode
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
              if (for (l <- (0 until m - 1).iterator) yield if (rowLengths(l) == j + 1) { newGaps(l) == 0 } else { newGaps(l) >= 0 }).forall(_ == true);
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

    // we need to filter the results; if M wasn't positive definite there are spurious answers.
    val result = partialDecompositions(M.numberOfRows).filter(A => integerMatrices.compose(A, A.transpose) == M)

    import CholeskyDecomposition._
    if (result.nonEmpty && !M.mapEntries(_.toDouble).positiveSemidefinite_?) {
      println("positiveSemidefinite_? seems to fail on:")
      println(M)
      throw new IllegalArgumentException("positiveSemidefinite_? failed on " + M)
    }

    result

    // here's a newer implementation, but that seems to be broken
    //    PositiveSymmetricDecomposition(M.entries.toArray.map(_.toArray)).decompositions.map(Matrix.fromArray)
  }

}