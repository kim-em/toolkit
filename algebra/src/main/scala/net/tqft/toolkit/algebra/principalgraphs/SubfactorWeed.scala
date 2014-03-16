package net.tqft.toolkit.algebra.principalgraphs

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.permutations.Permutations._
import net.tqft.toolkit.permutations.Involutions
import net.tqft.toolkit.permutations.Involutions._
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.enumeration.Odometer
import net.tqft.toolkit.algebra.graphs._
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.collections.Orderings._
import net.tqft.toolkit.collections.LexicographicOrdering._
import net.tqft.toolkit.collections.Tally._
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import scala.collection.mutable.ListBuffer

trait Memo[+T] {
  def value: T
  def map[S](f: T => S): Memo[S] = Memo(f(value))
  def computed: Boolean
  override def toString = "Memo(" + (if (computed) value.toString else "---not computed yet---") + ")"
  override def hashCode = value.hashCode
  override def equals(other: Any) = other match {
    case other: Memo[T] => value == other.value
    case _ => false
  }
}

object Memo {
  def apply[T](function: => T): Memo[T] = {
    new Memo[T] {
      private var _computed = false;
      override lazy val value = { val v = function; _computed = true; v }
      override def computed = _computed
    }
  }
}

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
      case k if k < 0 => inclusions(depth + k).size
      case k if k > 0 => inclusions(k - 1).size
    }
  }
  def valence(k: Int, index: Int) = {
    if (k == 0) {
      inclusions.head.map(_(index)).sum
    } else if (k == depth) {
      inclusions.last(index).sum
    } else {
      inclusions(k - 1).map(_(index)).sum + inclusions(k)(index).sum
    }
  }
  lazy val totalRank: Int = (for (k <- 0 to depth) yield rankAtDepth(k)).sum
  lazy val totalEvenRank: Int = (for (k <- 0 to depth by 2) yield rankAtDepth(k)).sum
  def depth: Int = inclusions.size
  lazy val supertransitivity = inclusions.indexWhere(_ != Seq(Seq(1))) match {
    case -1 => depth
    case k => k
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

      var m = 5
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
}

trait BigraphWithDuals {
  def bigraph: Bigraph
  def dualData: Seq[Involution]

  override def toString = "bwd" + bigraph.toString.stripPrefix("gbg") + "duals" + dualData.map(_.map(_ + 1).mkString("x")).mkString("v")

  def truncate: BigraphWithDuals
  def increaseDepth: BigraphWithDuals
}

case class EvenDepthBigraphWithDuals(bigraph: Bigraph, dualData: Seq[Involution]) extends BigraphWithDuals {
  require(bigraph.depth % 2 == 0)
  require(bigraph.depth / 2 + 1 == dualData.size)

  def addSelfDualVertex(row: Seq[Int]): EvenDepthBigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    EvenDepthBigraphWithDuals(bigraph.addRow(row), dualData.most :+ (dualData.last :+ bigraph.rankAtMaximalDepth))
  }
  def deleteSelfDualVertex(index: Int): EvenDepthBigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    require(dualData.last(index) == index)
    EvenDepthBigraphWithDuals(bigraph.deleteRow(index), dualData.most :+ dualData.last.collect({ case i if i < index => i; case i if i > index => i - 1 }))
  }
  def addDualPairOfVertices(row0: Seq[Int], row1: Seq[Int]): EvenDepthBigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    EvenDepthBigraphWithDuals(bigraph.addRow(row0).addRow(row1), dualData.most :+ (dualData.last :+ (bigraph.rankAtMaximalDepth + 1) :+ bigraph.rankAtMaximalDepth))
  }
  def deleteDualPairOfVertices(index: Int): EvenDepthBigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    require(dualData.last(index) == index + 1)
    EvenDepthBigraphWithDuals(bigraph.deleteRow(index).deleteRow(index), dualData.most :+ dualData.last.collect({ case i if i < index => i; case i if i > index + 1 => i - 2 }))
  }

  // this checks if a row can be added to the dual graph, satisfying the associativity condition for paths from vertices at depth n-2 on _this_ graph to the new vertex on the _dual_ graph
  def rowAllowed_?(row: Seq[Int]): Boolean = {
    (for (i <- (0 until bigraph.rankAtDepth(-3)).iterator) yield {
      (for ((r, j) <- bigraph.inclusions.secondLast.zipWithIndex) yield (r(i) - r(dualData.secondLast(i))) * row(j)).sum
    }).forall(_ == 0)
  }
  // this checks if a pair of rows can be added to the dual graph, satisfying the associativity condition for paths from vertices at depth n-2 on _this_ graph to the new vertices on the _dual_ graph
  def rowsAllowed_?(row0: Seq[Int], row1: Seq[Int]): Boolean = {
    (for (i <- (0 until bigraph.rankAtDepth(-3)).iterator) yield {
      var s = 0
      var t = 0
      for ((r, j) <- bigraph.inclusions.secondLast.zipWithIndex) {
        s += r(i) * row0(j) - r(dualData.secondLast(i)) * row1(j)
        t += r(i) * row1(j) - r(dualData.secondLast(i)) * row0(j)
      }
      (s, t)
    }).forall(_ == (0, 0))
  }

  override def truncate = OddDepthBigraphWithDuals(bigraph.truncate, dualData.most)
  override def increaseDepth = OddDepthBigraphWithDuals(bigraph.increaseDepth, dualData)

}

case class OddDepthBigraphWithDuals(bigraph: Bigraph, dualData: Seq[Involution]) extends BigraphWithDuals {
  require(bigraph.depth % 2 == 1)
  require((bigraph.depth + 1) / 2 == dualData.size)

  def addOddVertex(row: Seq[Int]) = copy(bigraph.addRow(row))
  def deleteOddVertex(index: Int) = copy(bigraph.deleteRow(index))

  override def truncate = EvenDepthBigraphWithDuals(bigraph.truncate, dualData)
  override def increaseDepth = EvenDepthBigraphWithDuals(bigraph.increaseDepth, dualData :+ IndexedSeq.empty)
}

case class Vertex(graph: Int, depth: Int, index: Int)
case class TriplePointConfiguration(depth: Int, index0: Int, index1: Int, bijection: Seq[(Vertex, Vertex)])

trait PairOfBigraphsWithDuals {
  def g0: BigraphWithDuals
  def g1: BigraphWithDuals
  def associativityDefects: Memo[Seq[Seq[Int]]]
  def verticesByDimension: Memo[Seq[Seq[Seq[Int]]]]

  def triplePointConfigurations: Memo[Seq[TriplePointConfiguration]]

  def passesTriplePointObstruction_? = triplePointConfigurations.value.exists({ conf =>
    val vertexClumps = verticesByDimension.value
    conf.bijection.forall({
      case (v1, v2) =>
        vertexClumps(v1.graph)(v1.depth)(v1.index) == vertexClumps(v2.graph)(v2.depth)(v2.index)
    })
  })

  protected def updateVerticesByDimensions(vertexClumps: Seq[Seq[Seq[Int]]]): Seq[Seq[Seq[Int]]] = ???

  override def toString = s"{ $g0, $g1 }"

  def depth = g0.bigraph.depth
  def totalRank = g0.bigraph.totalRank + g1.bigraph.totalRank

  override def hashCode() = (g0, g1).hashCode
  override def equals(other: Any) = other match {
    case p: PairOfBigraphsWithDuals => p.g0 == g0 && p.g1 == g1
    case _ => false
  }

  def apply(i: Int): BigraphWithDuals
  def associativeAtPenultimateDepth_? = {
    defectsZero_?(associativityDefects.value)
  }

  def truncate: PairOfBigraphsWithDuals
  def increaseDepth: PairOfBigraphsWithDuals

  protected def defectsZero_?(defects: Seq[Seq[Int]]) = defects.forall(_.forall(_ == 0))

  lazy val rankPartialSums = (for (graph <- 0 to 1; d <- 0 to apply(graph).bigraph.depth) yield apply(graph).bigraph.rankAtDepth(d)).scanLeft(0)(_ + _)
  def graphLabel(graph: Int, depth: Int, index: Int): Int = {
    //    require(index < pair(graph).bigraph.rankAtDepth(depth))
    val result = rankPartialSums(graph * (g0.bigraph.depth + 1) + depth) + index
    //    println(s"graphLabel($graph, $depth, $index) = $result")
    result
  }

  lazy val nautyGraph: ColouredGraph[(Int, Int)] = {
    // FIXME this ignores edge multiplicities!
    val adjacencies = (
      for (
        graph <- 0 to 1;
        g = apply(graph);
        d <- 0 to depth;
        i <- 0 until g.bigraph.rankAtDepth(d)
      ) yield {
        (if (d % 2 == 0) {
          val j = g.dualData(d / 2)(i)
          if (i == j) {
            Seq.empty
          } else {
            Seq(graphLabel(graph, d, j))
          }
        } else {
          Seq(graphLabel(1 - graph, d, i))
        }) ++
          (if (d == g.bigraph.depth) {
            Seq.empty
          } else {
            (for (
              j <- 0 until g.bigraph.rankAtDepth(d + 1);
              if g.bigraph.inclusions(d)(j)(i) > 0
            ) yield {
              graphLabel(graph, d + 1, j)
            })
          })
      }).toIndexedSeq
    require(adjacencies.forall(_.forall(_ < totalRank)))
    val colours = for (
      graph <- 0 to 1;
      d <- 0 to depth;
      i <- 0 until apply(graph).bigraph.rankAtDepth(d)
    ) yield (graph, d)
    Graph(totalRank, adjacencies).colour(colours)
  }

}

object PairOfBigraphsWithDuals {
  private def empty(duals0: Involution, duals1: Involution) = {
    val clumps: Seq[Seq[Seq[Int]]] = Seq(
      Seq(for (i <- 0 until duals0.size) yield (i + duals0(i) / 2)),
      Seq(for (i <- 0 until duals1.size) yield duals0.size + (i + duals1(i) / 2)))

    EvenDepthPairOfBigraphsWithDuals(
      EvenDepthBigraphWithDuals(Bigraph.empty(duals0.size), Seq(duals0)),
      EvenDepthBigraphWithDuals(Bigraph.empty(duals1.size), Seq(duals1)),
      associativityDefects = Memo(Seq.empty[Seq[Int]]),
      verticesByDimension = Memo(clumps),
      triplePointConfigurations = Memo(Seq.empty[TriplePointConfiguration]))
  }

  def apply(g0: BigraphWithDuals, g1: BigraphWithDuals): PairOfBigraphsWithDuals = {
    val graphs = List(g0, g1)

    var evenDepthScratch: EvenDepthPairOfBigraphsWithDuals = PairOfBigraphsWithDuals.empty(g0.dualData(0), g1.dualData(0))
    var oddDepthScratch: OddDepthPairOfBigraphsWithDuals = null
    for (workingDepth <- 1 to g0.bigraph.depth) {
      if (workingDepth % 2 == 1) {
        oddDepthScratch = evenDepthScratch.increaseDepth
        for (i <- 0 until g0.bigraph.rankAtDepth(workingDepth)) {
          oddDepthScratch = oddDepthScratch.addDualPairAtOddDepth(g0.bigraph.inclusions(workingDepth - 1)(i), g1.bigraph.inclusions(workingDepth - 1)(i)).get
        }
      } else {
        evenDepthScratch = oddDepthScratch.increaseDepth
        for (graph <- 0 to 1; i <- 0 until graphs(graph).bigraph.rankAtDepth(workingDepth)) {
          val j = graphs(graph).dualData(workingDepth / 2)(i)
          if (i == j) {
            evenDepthScratch = evenDepthScratch.addSelfDualVertex(graph, graphs(graph).bigraph.inclusions(workingDepth - 1)(i)).get
          } else if (j == i + 1) {
            evenDepthScratch = evenDepthScratch.addDualPairAtEvenDepth(graph, graphs(graph).bigraph.inclusions(workingDepth - 1)(i), graphs(graph).bigraph.inclusions(workingDepth - 1)(i + 1)).get
          } else if (j == i - 1) {
            // do nothing, the previous case has already added this row
          } else {
            ???
          }
        }
      }
    }
    if (g0.bigraph.depth % 2 == 0) {
      evenDepthScratch
    } else {
      oddDepthScratch
    }

    //  def computeVerticesByDimensions(g0: BigraphWithDuals, g1: BigraphWithDuals) = {
    //    val depth = g0.bigraph.depth
    //    val graphs = List(g0, g1)
    //    val dualClumps = {
    //      var m = 0
    //      for (graph <- 0 to 1) yield {
    //        for (d <- 0 to depth) yield {
    //          for (k <- 0 until graphs(graph).bigraph.rankAtDepth(d)) yield {
    //            ???
    //          }
    //        }
    //      }
    //    }
    //    updateVerticesByDimensions(g0, g1, dualClumps)
    //  }
    //
    //  def updateVerticesByDimensions(g0: BigraphWithDuals, g1: BigraphWithDuals, clumps: Seq[Seq[Seq[Int]]]) = {
    //    ???
    //  }
    //
    //  def computeTriplePoints(g0: BigraphWithDuals, g1: BigraphWithDuals): Seq[TriplePointConfiguration] = {
    //    def pathsBetween(depth: Int, index0: Int, index1: Int): Int = {
    //      ???
    //    }
    //
    //    for (
    //      d <- 0 until g0.bigraph.depth - 3;
    //      i <- 0 until g0.bigraph.rankAtDepth(d);
    //      j <- 0 until g1.bigraph.rankAtDepth(d);
    //      if g0.bigraph.valence(d, i) == 3;
    //      if g1.bigraph.valence(d, j) == 3;
    //      if false
    //    ) yield {
    //      ???
    //    }
    //  }

  }
}
case class EvenDepthPairOfBigraphsWithDuals(g0: EvenDepthBigraphWithDuals, g1: EvenDepthBigraphWithDuals, associativityDefects: Memo[Seq[Seq[Int]]], verticesByDimension: Memo[Seq[Seq[Seq[Int]]]], triplePointConfigurations: Memo[Seq[TriplePointConfiguration]]) extends PairOfBigraphsWithDuals {
  require(depth == 0 || associativityDefects.value.size == g0.bigraph.rankAtDepth(-2))
  require(depth == 0 || associativityDefects.value.head.size == g1.bigraph.rankAtDepth(-2))

  override def apply(i: Int): EvenDepthBigraphWithDuals = i match {
    case 0 => g0
    case 1 => g1
  }

  override def associativeAtPenultimateDepth_? = {
    defectsZero_?(associativityDefects.value)
  }

  override def truncate = OddDepthPairOfBigraphsWithDuals(g0.truncate, g1.truncate)
  override def increaseDepth = OddDepthPairOfBigraphsWithDuals(g0.increaseDepth, g1.increaseDepth, ???, ???, ???)

  private def checkDefect(graph: Int, defect: Seq[Seq[Int]]) = {
    (graph == 0 || defect.forall(_.forall(_ >= 0)))
    // TODO if a defect is large, we still need to attach lots of edges, which might be enough to push the eigenvalue up too high.
    // TODO actually, this should be part of the odometer limit function
  }

  def addSelfDualVertex(graph: Int, row: Seq[Int]): Option[EvenDepthPairOfBigraphsWithDuals] = {
    val defects = associativityDefects.map({ defects =>
      val sign = if (graph == 0) 1 else -1
      for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
        for (j <- 0 until g1.bigraph.rankAtDepth(-2)) yield {
          defects(i)(j) + sign * row(i) * row(j)
        }
      }
    })
    def vertexClumps = verticesByDimension.map({ oldClumps =>
      updateVerticesByDimensions({
        ???
      })
    })
    (apply(1 - graph).rowAllowed_?(row) && checkDefect(graph, defects.value)) option {
      graph match {
        case 0 => EvenDepthPairOfBigraphsWithDuals(g0.addSelfDualVertex(row), g1, defects, vertexClumps, triplePointConfigurations)
        case 1 => EvenDepthPairOfBigraphsWithDuals(g0, g1.addSelfDualVertex(row), defects, vertexClumps, triplePointConfigurations)
      }
    }
  }
  def addDualPairAtEvenDepth(graph: Int, row0: Seq[Int], row1: Seq[Int]): Option[EvenDepthPairOfBigraphsWithDuals] = {
    val defects = associativityDefects.map({ defects =>
      val sign = if (graph == 0) 1 else -1
      for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
        for (j <- 0 until g1.bigraph.rankAtDepth(-2)) yield {
          defects(i)(j) + sign * (row0(i) * row1(j) + row1(i) * row0(j))
        }
      }
    })
    def vertexClumps = verticesByDimension.map({ oldClumps =>
      updateVerticesByDimensions({
        val z = ???
        oldClumps.updated(graph, oldClumps(graph).updated(depth, oldClumps(graph)(depth) ++ Seq(z, z)))
      })
    })
    (apply(1 - graph).rowsAllowed_?(row0, row1) && checkDefect(graph, defects.value)) option {
      graph match {
        case 0 => EvenDepthPairOfBigraphsWithDuals(g0.addDualPairOfVertices(row0, row1), g1, defects, vertexClumps, triplePointConfigurations)
        case 1 => EvenDepthPairOfBigraphsWithDuals(g0, g1.addDualPairOfVertices(row0, row1), defects, vertexClumps, triplePointConfigurations)
      }
    }
  }

  def deleteSelfDualVertex(graph: Int, index: Int): EvenDepthPairOfBigraphsWithDuals = graph match {
    case 0 => EvenDepthPairOfBigraphsWithDuals(g0.deleteSelfDualVertex(index), g1)
    case 1 => EvenDepthPairOfBigraphsWithDuals(g0, g1.deleteSelfDualVertex(index))
  }
  def deleteDualPairAtEvenDepth(graph: Int, index: Int): EvenDepthPairOfBigraphsWithDuals = graph match {
    case 0 => EvenDepthPairOfBigraphsWithDuals(g0.deleteDualPairOfVertices(index), g1)
    case 1 => EvenDepthPairOfBigraphsWithDuals(g0, g1.deleteDualPairOfVertices(index))
  }
}

case class OddDepthPairOfBigraphsWithDuals(g0: OddDepthBigraphWithDuals, g1: OddDepthBigraphWithDuals, associativityDefects: Memo[Seq[Seq[Int]]], verticesByDimension: Memo[Seq[Seq[Seq[Int]]]], triplePointConfigurations: Memo[Seq[TriplePointConfiguration]]) extends PairOfBigraphsWithDuals {
  override def apply(i: Int): OddDepthBigraphWithDuals = i match {
    case 0 => g0
    case 1 => g1
  }

  override def truncate = EvenDepthPairOfBigraphsWithDuals(g0.truncate, g1.truncate)
  override def increaseDepth = EvenDepthPairOfBigraphsWithDuals(g0.increaseDepth, g1.increaseDepth, ???, ???, ???)

  def addDualPairAtOddDepth(row0: Seq[Int], row1: Seq[Int]): Option[OddDepthPairOfBigraphsWithDuals] = {
    val allowed = {
      g0.bigraph.depth == 1 ||
        (for (i <- (0 until g0.bigraph.rankAtDepth(-3)).iterator) yield {
          (for ((r0, r1) <- g0.bigraph.inclusions.secondLast.zip(g0.bigraph.inclusions.secondLast)) yield r1(i) * row0(g1.dualData.last(i)) - r0(i) * row1(g0.dualData.last(i))).sum
        }).forall(_ == 0)
    }
    def defects = associativityDefects.map({ oldDefects =>
      def row(i: Int) = i match {
        case 0 => row0
        case 1 => row1
      }
      for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
        for (j <- 0 until g1.bigraph.rankAtDepth(-2)) yield {
          oldDefects(i)(j) + row0(i) * row1(j) - row1(i) * row0(j)
        }
      }
    })
    def vertexClumps = verticesByDimension.map({ oldClumps =>
      updateVerticesByDimensions({
        ???
      })
    })
    allowed option
      OddDepthPairOfBigraphsWithDuals(g0.addOddVertex(row0), g1.addOddVertex(row1), associativityDefects = defects, verticesByDimension = vertexClumps, triplePointConfigurations)
  }
  def deleteDualPairAtOddDepth(index: Int) = OddDepthPairOfBigraphsWithDuals(g0.deleteOddVertex(index), g1.deleteOddVertex(index))
}

object EvenDepthPairOfBigraphsWithDuals {
  def apply(g0: EvenDepthBigraphWithDuals, g1: EvenDepthBigraphWithDuals): EvenDepthPairOfBigraphsWithDuals = {
    PairOfBigraphsWithDuals(g0, g1) match {
      case r: EvenDepthPairOfBigraphsWithDuals => r
    }
  }

  //    def defects = for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
  //      for (j <- 0 until g1.bigraph.rankAtDepth(-2)) yield {
  //        val `m*m*`: Int = {
  //          val updown: Int = {
  //            val inclusion = g0.bigraph.inclusions.last
  //            val duals = g0.dualData.last
  //            (for (k <- 0 until g0.bigraph.rankAtMaximalDepth) yield inclusion(k)(i) * inclusion(duals(k))(j)).sum
  //          }
  //          val downup: Int = {
  //            val inclusion = g0.bigraph.inclusions.secondLast
  //            val duals = g0.dualData.secondLast
  //            (for (k <- 0 until g0.bigraph.rankAtDepth(-3)) yield inclusion(i)(k) * inclusion(j)(duals(k))).sum
  //          }
  //          updown + downup
  //        }
  //        val `*m*m`: Int = {
  //          val updown: Int = {
  //            val inclusion = g1.bigraph.inclusions.last
  //            val duals = g1.dualData.last
  //            (for (k <- 0 until g1.bigraph.rankAtMaximalDepth) yield inclusion(k)(i) * inclusion(duals(k))(j)).sum
  //          }
  //          val downup: Int = {
  //            val inclusion = g1.bigraph.inclusions.secondLast
  //            val duals = g1.dualData.secondLast
  //            (for (k <- 0 until g1.bigraph.rankAtDepth(-3)) yield inclusion(i)(k) * inclusion(j)(duals(k))).sum
  //          }
  //          updown + downup
  //        }
  //        `m*m*` - `*m*m`
  //      }
  //    }
  //    EvenDepthPairOfBigraphsWithDuals(g0, g1,
  //      Memo(defects),
  //      Memo(PairOfBigraphsWithDuals.computeVerticesByDimensions(g0, g1)),
  //      Memo(PairOfBigraphsWithDuals.computeTriplePoints(g0, g1)))
}

object OddDepthPairOfBigraphsWithDuals {
  def apply(g0: OddDepthBigraphWithDuals, g1: OddDepthBigraphWithDuals): OddDepthPairOfBigraphsWithDuals = {
    PairOfBigraphsWithDuals(g0, g1) match {
      case r: OddDepthPairOfBigraphsWithDuals => r
    }
  }

  //  def apply(g0: OddDepthBigraphWithDuals, g1: OddDepthBigraphWithDuals): OddDepthPairOfBigraphsWithDuals = {
  //    def defects = {
  //      if (g0.bigraph.depth == 1 && g1.bigraph.depth == 1 && g0.bigraph.rankAtDepth(1) == 0 && g1.bigraph.rankAtDepth(1) == 0) {
  //        Seq.fill(g0.bigraph.rankAtDepth(0))(Seq.fill(g1.bigraph.rankAtDepth(0))(0))
  //      } else {
  //        for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
  //          for (j <- 0 until g1.bigraph.rankAtDepth(-2)) yield {
  //            val `m*m*`: Int = {
  //              val updown: Int = {
  //                val inclusion0 = g0.bigraph.inclusions.last
  //                val inclusion1 = g1.bigraph.inclusions.last
  //                val duals = g1.dualData.last
  //                (for (k <- 0 until g0.bigraph.rankAtMaximalDepth) yield inclusion0(k)(i) * inclusion1(k)(duals(j))).sum
  //              }
  //              val downup: Int = {
  //                val inclusion0 = g0.bigraph.inclusions.secondLast
  //                val inclusion1 = g1.bigraph.inclusions.secondLast
  //                val duals = g1.dualData.last
  //                (for (k <- 0 until g0.bigraph.rankAtDepth(-3)) yield inclusion0(i)(k) * inclusion1(duals(j))(k)).sum
  //              }
  //              updown + downup
  //            }
  //            val `*m*m`: Int = {
  //              val updown: Int = {
  //                val inclusion0 = g0.bigraph.inclusions.last
  //                val inclusion1 = g1.bigraph.inclusions.last
  //                val duals = g0.dualData.last
  //                (for (k <- 0 until g1.bigraph.rankAtMaximalDepth) yield inclusion0(k)(duals(i)) * inclusion1(k)(j)).sum
  //              }
  //              val downup: Int = {
  //                val inclusion0 = g0.bigraph.inclusions.secondLast
  //                val inclusion1 = g1.bigraph.inclusions.secondLast
  //                val duals = g0.dualData.last
  //                (for (k <- 0 until g1.bigraph.rankAtDepth(-3)) yield inclusion0(duals(i))(k) * inclusion1(j)(k)).sum
  //              }
  //              updown + downup
  //            }
  //            `m*m*` - `*m*m`
  //          }
  //        }
  //      }
  //    }
  //    OddDepthPairOfBigraphsWithDuals(g0, g1, Memo(defects), Memo(???), Memo(???))
  //  }
}

sealed trait SubfactorWeed extends CanonicalGeneration[SubfactorWeed, Seq[(Permutation, Permutation)]] { weed =>
  def indexLimit: Double
  def pair: PairOfBigraphsWithDuals

  override def toString = s"weed[$indexLimit, $pair]"

  def supertransitivity = pair(0).bigraph.supertransitivity

  def descendantsWithSupertransitivityAtMost(k: Int) = descendants(w => if (k >= w.supertransitivity) 1 else -1)

  override def findIsomorphismTo(other: SubfactorWeed) = ???
  def isomorphs = ???

  protected def depth = pair(0).bigraph.depth

  override lazy val automorphisms: FinitelyGeneratedFiniteGroup[Seq[(Permutation, Permutation)]] = {
    val group = Dreadnaut.automorphismGroup(pair.nautyGraph)

    def splitPermutation(p: Permutation): Seq[(Permutation, Permutation)] = {
      require(p(0) == 0)
      def shift(x: IndexedSeq[Int]) = {
        if (x.isEmpty) {
          x
        } else {
          val m = x.min
          x.map(_ - m)
        }
      }
      val chunks = pair.rankPartialSums.sliding(2).map(t => p.slice(t(0), t(1))).map(shift).toIndexedSeq
      val result = for (i <- 0 to depth) yield {
        (chunks(i), chunks(i + depth + 1))
      }
      for (i <- 0 to depth) {
        require(result(i)._1.sorted == (0 until pair(0).bigraph.rankAtDepth(i)))
        require(result(i)._2.sorted == (0 until pair(1).bigraph.rankAtDepth(i)))
      }
      result
    }

    FiniteGroups.indexedProduct(
      for (i <- 0 until pair.g0.bigraph.depth) yield {
        FiniteGroups.product(
          FiniteGroups.symmetricGroup(pair.g0.bigraph.rankAtDepth(i)),
          FiniteGroups.symmetricGroup(pair.g1.bigraph.rankAtDepth(i)))
      }).subgroupGeneratedBy(group.generators.map(splitPermutation))
  }

  sealed trait Upper {
    val result: SubfactorWeed
    def inverse: result.Lower

    def associative_? : Boolean
  }
  case object IncreaseDepth extends Upper {
    override lazy val result = SubfactorWeed(indexLimit, pair.increaseDepth)
    override def inverse = result.DecreaseDepth

    override def associative_? = pair.associativeAtPenultimateDepth_?
  }

  def decreaseDepthSet: Option[Set[Lower]] = (pair.g0.bigraph.rankAtMaximalDepth == 0 && pair.g1.bigraph.rankAtMaximalDepth == 0) option Set(DecreaseDepth)

  sealed trait Lower {
    def result: SubfactorWeed
  }
  case object DecreaseDepth extends Lower {
    override def result = SubfactorWeed(indexLimit, pair.truncate)
  }
}

object SubfactorWeed {
  def apply(indexLimit: Double, pair: PairOfBigraphsWithDuals): SubfactorWeed /* removing this return type crashes the 2.10.3 compiler; tell someone */ = {
    pair match {
      case pair: EvenDepthPairOfBigraphsWithDuals => EvenDepthSubfactorWeed(indexLimit, pair)
      case pair: OddDepthPairOfBigraphsWithDuals => OddDepthSubfactorWeed(indexLimit, pair)
    }
  }
}

case class EvenDepthSubfactorWeed(indexLimit: Double, pair: EvenDepthPairOfBigraphsWithDuals) extends SubfactorWeed { weed =>

  override lazy val lowerObjects = {
    new automorphisms.Action[Lower] {
      override val elements: Iterable[Lower] =
        decreaseDepthSet.getOrElse(
          (for (i <- 0 to 1; k <- 0 until pair(i).bigraph.rankAtMaximalDepth; if pair(i).dualData.last(k) != k - 1) yield {
            if (pair(i).dualData.last(k) == k) {
              DeleteSelfDualVertex(i, k)
            } else {
              DeleteDualPairAtEvenDepth(i, k)
            }
          }))

      override def act(g: Seq[(Permutation, Permutation)], x: Lower): Lower = x match {
        case DecreaseDepth => x
        case DeleteSelfDualVertex(graph, index) => DeleteSelfDualVertex(graph, (graph match { case 0 => g.last._1; case 1 => g.last._2 })(index))
        case DeleteDualPairAtEvenDepth(graph, index) => {
          val newIndex0 = {
            (graph match { case 0 => g.last._1; case 1 => g.last._2 })(index)
          }
          val newIndex1 = if (pair(graph).dualData.last(newIndex0) < newIndex0) {
            newIndex0 - 1
          } else {
            newIndex0
          }
          DeleteDualPairAtEvenDepth(graph, newIndex1)
        }
      }
    }
  }
  override def ordering: Ordering[lowerObjects.Orbit] = {
    implicit val lowerOrdering: Ordering[Lower] = Ordering.by({ l: Lower =>
      l match {
        case DecreaseDepth => 0
        case DeleteDualPairAtEvenDepth(1, _) => 1
        case DeleteSelfDualVertex(1, _) => 2
        case DeleteDualPairAtEvenDepth(0, _) => 3
        case DeleteSelfDualVertex(0, _) => 4
      }
    }).refineByPartialFunction({
      case DeleteSelfDualVertex(graph, index) => Dreadnaut.canonicalize(pair.nautyGraph.additionalMarking(Seq(pair.graphLabel(graph, pair(graph).bigraph.depth, index))))
      case DeleteDualPairAtEvenDepth(graph, index) => Dreadnaut.canonicalize(pair.nautyGraph.additionalMarking(Seq(pair.graphLabel(graph, pair(graph).bigraph.depth, index), pair.graphLabel(graph, pair(graph).bigraph.depth, index + 1))))
    })
    Ordering.by({ o: lowerObjects.Orbit => o.representative })
  }

  override def upperObjects = {
    new automorphisms.Action[Upper] {
      override val elements: Iterable[Upper] = {
        val allUppers: Iterable[Upper] = {
          def uppersAddingVerticesToGraph(graph: Int) = {
            if (graph == 0 && pair.g1.bigraph.rankAtMaximalDepth > 0) {
              Seq.empty
            } else {
              uppersAddingSelfDualVerticesToGraph(graph) ++
                uppersAddingDualPairVerticesToGraph(graph)
            }
          }

          def uppersAddingSelfDualVerticesToGraph(graph: Int) = {
            val duals = pair(graph).dualData.last
            if (duals.nonEmpty && duals.last == duals.size - 2 /* graph has dual pairs already */ ) {
              Seq.empty
            } else {
              // Now we have to run an odometer to produce all values of the row.
              // We certainly give this odometer the index limit.
              // What sort of limits can we give the odometer?
              // --->  Given any function f: Seq[Int] => (L: Ordering) which is pointwise increasing and which is automorphism invariant,
              //       we can use the limit f(row) <= rows.map(f).min
              // Examples:
              // * sum of the entries
              // * tally of neighbour degrees (if vertex i at the previous depth has valence n_i, we send row to (n \mapsto \sum_{i: n_i = n} row_i), ordered lexicographically
              // 
              // Of course, any limits that are used here, have to be reflected in the ordering of lowers.

              val limit = { row: List[Int] =>
                // FIXME we're only doing the simply laced case for now
                row.forall(_ <= 1) &&
                  pair(graph).bigraph.isEigenvalueWithRowBelow_?(indexLimit)(row)
              }

              val initial = List.fill(pair(graph).bigraph.rankAtDepth(-2))(0)

              Odometer(limit)(initial).filter(r => r.exists(_ > 0)).map(AddSelfDualVertex(graph, _))
            }
          }

          def uppersAddingDualPairVerticesToGraph(graph: Int) = {
            def limit(bigraph: Bigraph) = { row: List[Int] =>
              // FIXME we're only doing the simply laced case for now
              row.forall(_ <= 1) &&
                bigraph.isEigenvalueWithRowBelow_?(indexLimit)(row)
            }
            def secondLimit(row0: List[Int], bigraph: Bigraph) = { row1: List[Int] =>
              row1 <= row0 && limit(bigraph)(row1)
            }

            val initial = List.fill(pair(graph).bigraph.rankAtDepth(-2))(0)

            Odometer(limit(pair(graph).bigraph))(initial).filter(r => r.exists(_ > 0))
              .map(row => (row, pair(graph).bigraph.addRow(row)))
              .flatMap(p => Odometer(secondLimit(p._1, p._2))(initial).filter(r => r.exists(_ > 0)).map(r => (p._1, r)))
              .map(p => AddDualPairAtEvenDepth(graph, p._1, p._2))
          }

          ((pair.g0.bigraph.rankAtMaximalDepth > 0 && pair.g1.bigraph.rankAtMaximalDepth > 0) option IncreaseDepth) ++
            uppersAddingVerticesToGraph(0) ++
            uppersAddingVerticesToGraph(1)
        }

        allUppers.filter(_.associative_?).filter(_.result.pair.passesTriplePointObstruction_?)
      }
      override def act(g: Seq[(Permutation, Permutation)], x: Upper): Upper = x match {
        case IncreaseDepth => x
        case AddSelfDualVertex(graph, row) => AddSelfDualVertex(graph, (graph match { case 0 => g.secondLast._1; case 1 => g.secondLast._2 }).permute(row))
        case AddDualPairAtEvenDepth(graph, row0, row1) => {
          val p = (graph match { case 0 => g.secondLast._1; case 1 => g.secondLast._2 })
          AddDualPairAtEvenDepth(graph, p.permute(row0), p.permute(row1))
        }
      }
    }
  }

  sealed trait AddVertexUpper extends Upper {
    def pairOption: Option[EvenDepthPairOfBigraphsWithDuals]
    def associative_? = pairOption.nonEmpty
    override lazy val result = EvenDepthSubfactorWeed(indexLimit, pairOption.get)
  }

  case class AddSelfDualVertex(graph: Int, row: Seq[Int]) extends AddVertexUpper {
    override lazy val pairOption = pair.addSelfDualVertex(graph, row)
    override def inverse = result.DeleteSelfDualVertex(graph, pair(graph).bigraph.rankAtMaximalDepth)
  }
  case class AddDualPairAtEvenDepth(graph: Int, row0: Seq[Int], row1: Seq[Int]) extends AddVertexUpper {
    override lazy val pairOption = pair.addDualPairAtEvenDepth(graph, row0, row1)
    override def inverse = result.DeleteDualPairAtEvenDepth(graph, pair(graph).bigraph.rankAtMaximalDepth)
  }

  case class DeleteSelfDualVertex(graph: Int, index: Int) extends Lower {
    override def result = EvenDepthSubfactorWeed(indexLimit, pair.deleteSelfDualVertex(graph, index))
  }
  case class DeleteDualPairAtEvenDepth(graph: Int, index: Int) extends Lower {
    require(index < pair(graph).bigraph.rankAtMaximalDepth - 1)
    override def result = EvenDepthSubfactorWeed(indexLimit, pair.deleteDualPairAtEvenDepth(graph, index))
  }
}

case class OddDepthSubfactorWeed(indexLimit: Double, pair: OddDepthPairOfBigraphsWithDuals) extends SubfactorWeed { weed =>

  override lazy val lowerObjects = {
    new automorphisms.Action[Lower] {
      val elements: Iterable[Lower] =
        decreaseDepthSet.getOrElse(
          (for (k <- 0 until pair.g0.bigraph.rankAtMaximalDepth) yield DeleteDualPairAtOddDepth(k)).toSet)

      def act(g: Seq[(Permutation, Permutation)], x: Lower): Lower = x match {
        case DecreaseDepth => x
        case DeleteDualPairAtOddDepth(index: Int) => DeleteDualPairAtOddDepth(g.last._1(index))
      }
    }
  }
  override def ordering: Ordering[lowerObjects.Orbit] = {
    implicit val lowerOrdering: Ordering[Lower] = Ordering.by({ l: Lower =>
      l match {
        case DecreaseDepth => 0
        case DeleteDualPairAtOddDepth(_) => 1
      }
    }).refineByPartialFunction({
      case l @ DeleteDualPairAtOddDepth(index) => {
        Dreadnaut.canonicalize(pair.nautyGraph.additionalMarking(Seq(pair.graphLabel(0, pair(0).bigraph.depth, index), pair.graphLabel(1, pair(1).bigraph.depth, index))))
      }
    })
    Ordering.by({ o: lowerObjects.Orbit => o.representative })
  }

  override def upperObjects = {
    new automorphisms.Action[Upper] {
      override val elements: Iterable[Upper] = {
        def uppersAddingVerticesToGraph: Iterable[Upper] = {

          val limit = { rows: List[List[Int]] =>
            // TODO: surely we can be more efficient here; at least saving some answers
            rows.forall(_.forall(_ <= 1)) &&
              pair(0).bigraph.isEigenvalueWithRowBelow_?(indexLimit)(rows(0)) && pair(0).bigraph.isEigenvalueWithRowBelow_?(indexLimit)(rows(1))
          }
          val initial = for (i <- (0 to 1).toList) yield List.fill(pair(i).bigraph.rankAtDepth(-2))(0)
          Odometer(limit)(initial).filter(_.forall(_.exists(_ > 0))).map({ rows => AddDualPairAtOddDepth(rows(0), rows(1)) })
        }

        val allUppers: Iterable[Upper] = ((pair.g0.bigraph.rankAtMaximalDepth > 0) option IncreaseDepth) ++ uppersAddingVerticesToGraph

        allUppers.filter(_.associative_?).filter(_.result.pair.passesTriplePointObstruction_?)
      }
      override def act(g: Seq[(Permutation, Permutation)], x: Upper): Upper = x match {
        case IncreaseDepth => x
        case AddDualPairAtOddDepth(row0, row1) => {
          AddDualPairAtOddDepth(g.secondLast._1.permute(row0), g.secondLast._2.permute(row1))
        }
      }
    }
  }

  sealed trait AddVertexUpper extends Upper {
    def pairOption: Option[OddDepthPairOfBigraphsWithDuals]
    def associative_? = pairOption.nonEmpty
    override lazy val result = OddDepthSubfactorWeed(indexLimit, pairOption.get)
  }

  case class AddDualPairAtOddDepth(row0: Seq[Int], row1: Seq[Int]) extends AddVertexUpper {
    override lazy val pairOption = pair.addDualPairAtOddDepth(row0, row1)
    override def inverse = result.DeleteDualPairAtOddDepth(pair.g0.bigraph.rankAtMaximalDepth)
  }
  case class DeleteDualPairAtOddDepth(index: Int) extends Lower {
    override def result = OddDepthSubfactorWeed(indexLimit, pair.deleteDualPairAtOddDepth(index))
  }

}