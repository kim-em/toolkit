package net.tqft.toolkit.algebra.principalgraphs

import scala.collection.mutable.ListBuffer

case class Bigraph(rankAtDepthZero: Int, inclusions: Seq[Seq[Seq[Int]]], evenDepthTwoStepNeighbours: Memo[IndexedSeq[IndexedSeq[List[(Int, Int)]]]]) {
  override def hashCode = (rankAtDepthZero, inclusions).hashCode
  override def equals(other: Any) = other match {
    case other: Bigraph => rankAtDepthZero == other.rankAtDepthZero && inclusions == other.inclusions
    case _ => false
  }

  override def toString = {
    inclusions.map(_.map(_.mkString("x")).mkString("p")).mkString("gbg", "v", "")
  }

  def addRow(row: Seq[Int]) = Bigraph(
    rankAtDepthZero,
    inclusions = inclusions.most :+ (inclusions.last :+ row),
    evenDepthTwoStepNeighbours = evenDepthTwoStepNeighbours.map({ n =>
      if (depth % 2 == 0) {
        // FIXME
        val newVertex = (depth / 2, rankAtMaximalDepth)
        val newVertexNeighbours = ListBuffer[(Int, Int)]()
        n.updated(n.size - 2,
          for ((l, i) <- n.secondLast.zipWithIndex) yield {
            l ++ List.fill((for (j <- 0 until rankAtDepth(-2)) yield {
              val c = inclusions.secondLast(j)(i) * row(j)
              newVertexNeighbours ++= List.fill(c)((depth / 2 - 1, i))
              c
            }).sum)(newVertex)
          })
          .updated(n.size - 1,
            (for ((l, i) <- n.last.zipWithIndex) yield {
              l ++ List.fill((for (j <- 0 until rankAtDepth(-2)) yield {
                val c = inclusions.last(i)(j) * row(j)
                newVertexNeighbours ++= List.fill(c)((depth / 2, i))
                c
              }).sum)(newVertex)
            }) :+ (List.fill(row.map(s => s * s).sum)(newVertex)) ++ newVertexNeighbours)
      } else {
        n.updated(n.size - 1, (for ((l, i) <- n.last.zipWithIndex) yield {
          if (row(i) == 0) {
            l
          } else {
            l ++ row.zipWithIndex.flatMap(p => List.fill(p._1)((depth / 2, p._2)))
          }
        }))
      }
    }))
  def deleteRow(index: Int) = Bigraph(
    rankAtDepthZero,
    inclusions = inclusions.most :+ (inclusions.last.removed(index)))

  def rankAtMaximalDepth: Int = rankAtDepth(depth)
  def rankAtDepth(k: Int): Int = {
    k match {
      case 0 => rankAtDepthZero
      case k if k < 0 => rankAtDepth(depth + 1 + k)
      case k if k > 0 => inclusions(k - 1).size
    }
  }
  def valence(d: Int, index: Int) = {
    //    require(d >= 0)
    //    require(d <= depth)
    //    require(index >= 0)
    //    require(index < rankAtDepth(d))
    if (d == 0) {
      inclusions.head.map(_(index)).sum
    } else if (d == depth) {
      inclusions.last(index).sum
    } else {
      inclusions(d - 1)(index).sum + inclusions(d).map(_(index)).sum
    }
  }
  lazy val neighbours: Map[(Int, Int), Seq[(Int, Int)]] = {
    (for (d <- 0 to depth; i <- 0 until rankAtDepth(d)) yield {
      (d, i) ->
        (for (
          j <- -1 to 0;
          if d + j >= 0;
          if d + j < depth;
          m = inclusions(d + j);
          c = if (j == -1) m(i) else m.map(_(i));
          (multiplicity, k) <- c.zipWithIndex;
          p <- 0 until multiplicity
        ) yield {
          (d + 2 * j + 1, k)
        })
    }).toMap
  }
  def nextToNeighbours(d: Int, k: Int) = {
    (for((d1, k1) <- neighbours((d,k)); (d2,k2) <- neighbours((d1,k1))) yield (d2,k2)).sorted
  }
  def downDownNeighbours(d: Int, k: Int) = {
    val row = inclusions(d-1)(k)
    (for((1, j) <- row.zipWithIndex; (1, i) <- inclusions(d-2)(j).zipWithIndex) yield i)
  }
  def downUpNeighbours(d: Int, k: Int) = {
    val row = inclusions(d-1)(k)
    (for((1, j) <- row.zipWithIndex; i <- (0 until rankAtDepth(d)).filter(i => inclusions(d-1)(i)(j) == 1)) yield i)
  }
  lazy val totalRank: Int = (for (k <- 0 to depth) yield rankAtDepth(k)).sum
  lazy val totalEvenRank: Int = (for (k <- 0 to depth by 2) yield rankAtDepth(k)).sum
  def depth: Int = inclusions.size

  lazy val supertransitivity = inclusions.indexWhere(i => i != Seq(Seq(1)) && i != Seq.empty) match {
    case -1 => depth
    case k => k
  }
  lazy val cylindrical_? = {
    supertransitivity < depth &&
    (inclusions.last ++ inclusions.secondLast ++ inclusions.last.transpose ++ inclusions.secondLast.transpose).map(_.sum).forall(_ <= 1)
  }

  def truncate: Bigraph = Bigraph(rankAtDepthZero, inclusions.most)
  def increaseDepth: Bigraph = Bigraph(
    rankAtDepthZero,
    inclusions :+ Seq.empty,
    evenDepthTwoStepNeighbours.map({ n =>
      if (depth % 2 == 0) {
        n
      } else {
        n :+ IndexedSeq.empty
      }
    }))

  private lazy val evenRanks: Array[Int] = Array.tabulate(depth / 2 + 1)({ d =>
    rankAtDepth(2 * d)
  })

  private def initialApproximateEigenvector: Array[Array[Double]] = {
    val z = scala.math.sqrt(1.0 / (totalEvenRank + (if (depth % 2 == 0) 1 else 0)))
    Array.tabulate(depth / 2 + 1)({ d =>
      Array.fill(evenRanks(d) + (if (depth % 2 == 0 && d == depth / 2) 1 else 0))(z)
    })
  }

  private lazy val approximateEigenvectors = Array.fill(2)(initialApproximateEigenvector)
  private def switchApproximateEigenvectors {
    val z = approximateEigenvectors(0)
    approximateEigenvectors(0) = approximateEigenvectors(1)
    approximateEigenvectors(1) = z
  }

  def isEigenvalueWithRowBelow_?(indexLimit: Double)(row: Seq[Int]): Boolean = {
    if (row.forall(_ == 0)) {
      true
    } else {

      var m = 20
      var last = 0.0
      var cur = estimateEigenvalueWithRow(row)
      def close = {
        cur - last < 0.00001
      }
      while (m > 0 && !close && cur < indexLimit) {
        m -= 1
        last = cur
        cur = estimateEigenvalueWithRow(row)
      }
      m == 0 || close
    }
  }

  def estimateEigenvalue(k: Int = 1): Double = estimateEigenvalueWithRow(Seq.fill(rankAtDepth(-2))(0), k)

  def estimateEigenvalueWithRow(row: Seq[Int], k: Int = 1): Double = {
    if (k > 1) {
      (for (j <- 0 until k) yield estimateEigenvalueWithRow(row, 1)).last
    } else {
      // TODO consider trying out Brendan's estimates
      // (in fact, they should be used in generating children, not here)

      switchApproximateEigenvectors
      val x = approximateEigenvectors(0)
      val y = approximateEigenvectors(1)

      //      println("x = " + x.map(_.toSeq).toSeq)

      val n = evenDepthTwoStepNeighbours.value
      //      println(n)

      for (i <- 0 to depth / 2) {
        for (j <- 0 until evenRanks(i)) {
          var s = 0.0
          for ((d, k) <- n(i)(j)) s += x(d)(k)
          y(i)(j) = s
        }
      }
      val mx = x(depth / 2)
      val my = y(depth / 2)
      if (depth % 2 == 0) {
        val nx = x(depth / 2 - 1)
        val ny = y(depth / 2 - 1)
        // calculate the value at the new vertex
        my(rankAtMaximalDepth) = {
          var s = 0.0
          // contributions from other stuff at the same depth
          for (i <- 0 until rankAtMaximalDepth) {
            for (j <- 0 until rankAtDepth(-2)) {
              s += mx(i) * row(j) * inclusions.last(i)(j)
            }
          }
          // contributions from stuff two depths down
          for (i <- 0 until rankAtDepth(-3)) {
            for (j <- 0 until rankAtDepth(-2)) {
              s += nx(i) * row(j) * inclusions.secondLast(j)(i)
            }
          }
          // contributions from the new vertex
          s += row.map(z => z * z).sum * mx(rankAtMaximalDepth)
          s
        }
        // add the additional contributions to the other vertices at the same depth
        for (i <- 0 until rankAtMaximalDepth) my(i) = my(i) + (for (j <- 0 until rankAtDepth(-2)) yield row(j) * inclusions.last(i)(j)).sum * mx(rankAtMaximalDepth)
        // add the additional contributions to the vertices two depths down
        for (i <- 0 until rankAtDepth(-3)) ny(i) = ny(i) + (for (j <- 0 until rankAtDepth(-2)) yield row(j) * inclusions.secondLast(j)(i)).sum * mx(rankAtMaximalDepth)
      } else {
        for (j <- 0 until evenRanks(depth / 2)) {
          for (k <- 0 until evenRanks(depth / 2)) {
            my(j) = my(j) + row(j) * row(k) * mx(k)
          }
        }
      }
      val squaredNorm = y.map(r => r.map(z => z * z).sum).sum

      //      println("y = " + y.map(_.toSeq).toSeq)

      val norm = scala.math.sqrt(squaredNorm)
      for (i <- 0 to depth / 2) {
        for (j <- 0 until y(i).length) {
          y(i)(j) = y(i)(j) / norm
        }
      }

      //      println(norm)
      norm
    }
  }

}

object Bigraph {
  def empty(rankAtDepthZero: Int) = Bigraph(rankAtDepthZero, Seq.empty, Memo(IndexedSeq(IndexedSeq.fill(rankAtDepthZero)(Nil))))

  def apply(rankAtDepthZero: Int, inclusions: Seq[Seq[Seq[Int]]]): Bigraph = {
    inclusions.foldLeft(empty(rankAtDepthZero))({ (b, i) => i.foldLeft(b.increaseDepth)({ (b, r) => b.addRow(r) }) })
  }

  def apply(string: String): Bigraph = {
    require(string.startsWith("gbg"))
    val inclusions = string.stripPrefix("gbg").split_!("v").map(_.split_!("p").map(_.split_!("x").map(_.toInt)))
    val rankAtDepthZero = inclusions(0)(0).size
    apply(rankAtDepthZero, inclusions)
  }

  object Examples {
    val Haagerup = apply("gbg1v1v1v1p1v1x0p0x1v1x0p0x1")
    val dualHaagerup = apply("gbg1v1v1v1p1v1x0p1x0")
  }
}
