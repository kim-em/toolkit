package net.tqft.toolkit.algebra.graphs

object MatricesUpToSimultaneousRowAndColumnPermutations {
  def convertToGraphs[E: Ordering](matrices: Seq[Seq[Seq[E]]]): Seq[ColouredGraph[(Int, Int)]] = {
    if (matrices.isEmpty) return Seq.empty

    val n = matrices.head.size
    for (m <- matrices) {
      assert(m.size == n)
      for (r <- m) assert(r.size == n)
    }

    val entries = (for (m <- matrices; i <- 0 until n; j <- 0 until n; if i != j) yield m(i)(j)).sorted.distinct
    val entriesIndex = entries.zipWithIndex.toMap
    val diagonals = (for (m <- matrices; i <- 0 until n) yield m(i)(i)).sorted.distinct
    val diagonalsIndex = diagonals.zipWithIndex.toMap

    val d = scala.math.ceil(scala.math.log(entries.size + 1) / scala.math.log(2)).toInt

    for (m <- matrices) yield {
      val colours = for (i <- 0 until n; j <- 0 until d) yield (j, diagonalsIndex(m(i)(i))) // we colour by a pair, layer and diagonal entry
      def p(i: Int, j: Int): Int = d * i + j
      val adjacencies = for (i <- 0 until n; j <- 0 until d) yield {
        (if (j != d - 1) Seq(p(i, j + 1)) else Seq.empty) ++
          (for (k <- 0 until n; if k != i; if ((entriesIndex(m(i)(k)) + 1 >> j) % 2 == 1)) yield p(k,j))
      }
      ColouredGraph(n * d, adjacencies, colours)
    }
  }

  def findIsomorphisms[E: Ordering](matrices: Seq[Seq[Seq[E]]]): Seq[(Int, IndexedSeq[Int])] = {
    Dreadnaut.findIsomorphisms(convertToGraphs(matrices), true)
  }

  def isomorphism[E: Ordering](m1: Seq[Seq[E]], m2: Seq[Seq[E]]): Option[IndexedSeq[Int]] = {
    findIsomorphisms(Seq(m1, m2)).apply(1) match {
      case (0, p) => Some(p)
      case (1, _) => None
      case _ => ???
    }
  }

}