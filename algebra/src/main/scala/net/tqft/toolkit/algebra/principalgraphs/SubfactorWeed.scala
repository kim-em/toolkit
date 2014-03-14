package net.tqft.toolkit.algebra.principalgraphs

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.permutations.Permutation
import net.tqft.toolkit.permutations.Involutions
import net.tqft.toolkit.permutations.Involutions._
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import scalaz._
import Scalaz._
import net.tqft.toolkit.algebra.enumeration.Odometer

trait Memo[+T] {
  def value: T
  def map[S](f: T => S): Memo[S] = Memo(f(value))
}

object Memo {
  def apply[T](function: => T): Memo[T] = {
    new Memo[T] {
      lazy val value = function
    }
  }

  def fail = Memo(???)
}

case class Bigraph(rankAtDepthZero: Int, inclusions: Seq[Seq[Seq[Int]]], evenDepthTwoStepNeighbours: Memo[IndexedSeq[IndexedSeq[List[(Int, Int)]]]]) {
  def addRow(row: Seq[Int]) = Bigraph(
    rankAtDepthZero,
    inclusions = inclusions.most :+ (inclusions.last :+ row),
    evenDepthTwoStepNeighbours = evenDepthTwoStepNeighbours.map({ n =>
      if (depth % 2 == 0) {
        ???
      } else {
        ???
      }
    }))
  def deleteRow(index: Int) = Bigraph(
    rankAtDepthZero,
    inclusions = inclusions.most :+ (inclusions.last.removed(index)),
    evenDepthTwoStepNeighbours = Memo.fail)
  def rankAtMaximalDepth: Int = rankAtDepth(depth)
  def rankAtDepth(k: Int): Int = {
    k match {
      case 0 => rankAtDepthZero
      case k if k < 0 => rankAtDepth(depth + 1 + k)
      case k if k > 0 => inclusions(k - 1).size
    }
  }
  lazy val totalRank = (for (k <- 0 to depth) yield rankAtDepth(k)).sum
  lazy val totalEvenRank = (for (k <- 0 to depth by 2) yield rankAtDepth(k)).sum
  def depth: Int = inclusions.size

  def truncate: Bigraph = Bigraph(rankAtDepthZero, inclusions.most, Memo.fail)
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

  private lazy val evenRanks = Array.tabulate(depth / 2 + 1)({ d =>
      rankAtDepth(2 * d)
  })

  private def initialApproximateEigenvector = {
    val z = scala.math.sqrt(1.0 / (totalEvenRank + (if(depth % 2 == 0) 1 else 0)))
    Array.tabulate(depth / 2 + 1)({ d =>
      Array.fill(evenRanks(d) + (if(depth % 2 == 0 && d == depth / 2) 1 else 0))(z)
    })
  }
  
  private lazy val approximateEigenvectors = Array.fill(2)(initialApproximateEigenvector)
  private def switchApproximateEigenvectors {
    val z = approximateEigenvectors(0)
    approximateEigenvectors(0) = approximateEigenvectors(1)
    approximateEigenvectors(1) = z
  }
  

  def isEigenValueWithRowBelow_?(indexLimit: Double)(row: Seq[Int]): Boolean = {
    var m = 5
    while (m > 0 && estimateEigenvalueWithRow(row) < indexLimit) {
      m -= 1
    }
    m == 0
  }

  def estimateEigenvalueWithRow(row: Seq[Int]): Double = {
    // TODO consider trying out Brendan's estimates

    switchApproximateEigenvectors
    val x = approximateEigenvectors(0)
    val y = approximateEigenvectors(1)
    
    // FIXME this overwrites x too quickly!
    val n = evenDepthTwoStepNeighbours.value
    var squaredNorm = 0.0
    for (i <- 0 to depth / 2) {
      for (j <- 0 until evenRanks(i)) {
        var s = 0.0
        for ((d, k) <- n(i)(j)) s += x(d)(k)
        y(i)(j) = s
        squaredNorm += s * s
      }
    }
    val mx = x(depth / 2)
    val my = y(depth / 2)
    if (depth % 2 == 0) {
      val nx = x(depth / 2 - 1)
      val ny = y(depth / 2 - 1)
      my(rankAtMaximalDepth + 1) = ???
      for (j <- 0 until rankAtMaximalDepth) my(j) = mx(j) + { ???; 0.0 }
      for (j <- 0 until rankAtDepth(-1)) ny(j) = nx(j) + { ???; 0.0 }
    } else {
      for (j <- 0 until evenRanks(depth / 2)) {
        for (k <- 0 until evenRanks(depth / 2)) {
          my(j) = my(j) + row(j) * row(k) * mx(k)
        }
      }
    }
    var norm = scala.math.sqrt(squaredNorm)
    for (i <- 0 to depth / 2) {
      for (j <- 0 until evenRanks(i)) {
        y(i)(j) = y(i)(j) / norm
      }
    }
    squaredNorm

  }

}

object Bigraph {
  def apply(rankAtDepthZero: Int, inclusions: Seq[Seq[Seq[Int]]]): Bigraph = {
    inclusions.foldLeft(Bigraph(rankAtDepthZero, Seq.empty, Memo(IndexedSeq(IndexedSeq.fill(rankAtDepthZero)(Nil)))))({ (b, i) => i.foldLeft(b.increaseDepth)({ (b, r) => b.addRow(r) }) })
  }
}

trait BigraphWithDuals {
  def bigraph: Bigraph
  def dualData: Seq[Involution]

  def truncate: BigraphWithDuals
  def increaseDepth: BigraphWithDuals
}

case class EvenDepthBigraphWithDuals(bigraph: Bigraph, dualData: Seq[Involution]) extends BigraphWithDuals {
  require(bigraph.depth % 2 == 0)
  require(bigraph.depth / 2 + 1 == dualData.size)

  def addSelfDualVertex(row: Seq[Int]): EvenDepthBigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    EvenDepthBigraphWithDuals(bigraph.addRow(row), dualData.most :+ (dualData.last :+ (bigraph.rankAtMaximalDepth + 1)))
  }
  def deleteSelfDualVertex(index: Int): EvenDepthBigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    require(dualData.last(index) == index)
    EvenDepthBigraphWithDuals(bigraph.deleteRow(index), dualData.most :+ dualData.last.collect({ case i if i < index => i; case i if i > index => i - 1 }))
  }
  def addDualPairOfVertices(row1: Seq[Int], row2: Seq[Int]): EvenDepthBigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    EvenDepthBigraphWithDuals(bigraph.addRow(row1).addRow(row2), dualData.most :+ (dualData.last :+ (bigraph.rankAtMaximalDepth + 2) :+ (bigraph.rankAtMaximalDepth + 1)))
  }
  def deleteDualPairOfVertices(index: Int): EvenDepthBigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    require(dualData.last(index) == index + 1)
    EvenDepthBigraphWithDuals(bigraph.deleteRow(index).deleteRow(index), dualData.most :+ dualData.last.collect({ case i if i < index => i; case i if i > index + 1 => i - 2 }))
  }

  // this checks if a row can be added to the dual graph, satisfying the associativity condition for paths from vertices at depth n-2 on _this_ graph to the new vertex on the _dual_ graph
  def rowAllowed_?(row: Seq[Int]): Boolean = {
    (for (i <- (0 until bigraph.rankAtDepth(-2)).iterator) yield {
      var s = 0
      for (r <- bigraph.inclusions.secondLast) s += (r(i) - r(dualData.last(i))) * row(i)
      s
    }).forall(_ == 0)
  }
  // this checks if a pair of rows can be added to the dual graph, satisfying the associativity condition for paths from vertices at depth n-2 on _this_ graph to the new vertices on the _dual_ graph
  def rowsAllowed_?(row1: Seq[Int], row2: Seq[Int]): Boolean = {
    (for (i <- (0 until bigraph.rankAtDepth(-2)).iterator) yield {
      var s = 0
      var t = 0
      for (r <- bigraph.inclusions.secondLast) {
        s += r(i) * row1(i) - r(dualData.last(i)) * row2(i)
        t += r(i) * row2(i) - r(dualData.last(i)) * row1(i)
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

trait PairOfBigraphsWithDuals {
  def g0: BigraphWithDuals
  def g1: BigraphWithDuals

  override def hashCode() = (g0, g1).hashCode
  override def equals(other: Any) = other match {
    case p: PairOfBigraphsWithDuals => p.g0 == g0 && p.g1 == g1
    case _ => false
  }

  def apply(i: Int): BigraphWithDuals
  def associativeAtPenultimateDepth_? : Boolean

  def truncate: PairOfBigraphsWithDuals
  def increaseDepth: PairOfBigraphsWithDuals

  protected def defectsZero_?(defects: Seq[Seq[Int]]) = defects.forall(_.forall(_ == 0))
}

case class EvenDepthPairOfBigraphsWithDuals(g0: EvenDepthBigraphWithDuals, g1: EvenDepthBigraphWithDuals, associativityDefects: Memo[Seq[Seq[Int]]]) extends PairOfBigraphsWithDuals {

  override def apply(i: Int): EvenDepthBigraphWithDuals = i match {
    case 0 => g0
    case 1 => g1
  }

  override def associativeAtPenultimateDepth_? = {
    defectsZero_?(associativityDefects.value)
  }

  override def truncate = OddDepthPairOfBigraphsWithDuals(g0.truncate, g1.truncate, Memo.fail)
  override def increaseDepth = OddDepthPairOfBigraphsWithDuals(g0.increaseDepth, g1.increaseDepth)

  def addSelfDualVertex(graph: Int, row: Seq[Int]): Option[EvenDepthPairOfBigraphsWithDuals] = {
    // TODO: return None if associativityDefects are non-positive
    (apply(1 - graph).rowAllowed_?(row)) option {
      graph match {
        case 0 => copy(g0 = g0.addSelfDualVertex(row), associativityDefects = ???)
        case 1 => copy(g1 = g1.addSelfDualVertex(row), associativityDefects = ???)
      }
    }
  }
  def addDualPairAtEvenDepth(graph: Int, row1: Seq[Int], row2: Seq[Int]): Option[EvenDepthPairOfBigraphsWithDuals] = {
    // TODO: return None if associativityDefects are non-positive
    (apply(1 - graph).rowsAllowed_?(row1, row2)) option {
      graph match {
        case 0 => copy(g0 = g0.addDualPairOfVertices(row1, row2), associativityDefects = ???)
        case 1 => copy(g1 = g1.addDualPairOfVertices(row1, row2), associativityDefects = ???)
      }
    }
  }

  def deleteSelfDualVertex(graph: Int, index: Int): EvenDepthPairOfBigraphsWithDuals = graph match {
    case 0 => copy(g0 = g0.deleteSelfDualVertex(index), associativityDefects = Memo.fail)
    case 1 => copy(g1 = g1.deleteSelfDualVertex(index), associativityDefects = Memo.fail)
  }
  def deleteDualPairAtEvenDepth(graph: Int, index: Int): EvenDepthPairOfBigraphsWithDuals = graph match {
    case 0 => copy(g0 = g0.deleteDualPairOfVertices(index), associativityDefects = Memo.fail)
    case 1 => copy(g1 = g1.deleteDualPairOfVertices(index), associativityDefects = Memo.fail)
  }
}

case class OddDepthPairOfBigraphsWithDuals(g0: OddDepthBigraphWithDuals, g1: OddDepthBigraphWithDuals, associativityDefects: Memo[(Seq[Seq[Int]], Seq[Seq[Int]])]) extends PairOfBigraphsWithDuals {
  override def apply(i: Int): OddDepthBigraphWithDuals = i match {
    case 0 => g0
    case 1 => g1
  }

  override def associativeAtPenultimateDepth_? = {
    defectsZero_?(associativityDefects.value._1) && defectsZero_?(associativityDefects.value._2)
  }

  override def truncate = EvenDepthPairOfBigraphsWithDuals(g0.truncate, g1.truncate, Memo.fail)
  override def increaseDepth = EvenDepthPairOfBigraphsWithDuals(g0.increaseDepth, g1.increaseDepth)

  def addDualPairAtOddDepth(row1: Seq[Int], row2: Seq[Int]): Option[OddDepthPairOfBigraphsWithDuals] = {
    val allowed = {
      (for (i <- (0 until g0.bigraph.rankAtDepth(-2)).iterator) yield {
        var s = 0
        for ((r0, r1) <- g0.bigraph.inclusions.secondLast.zip(g0.bigraph.inclusions.secondLast)) s += r1(i) * row1(g1.dualData.last(i)) - r0(i) * row2(g0.dualData.last(i))
        s
      }).forall(_ == 0)
    }
    allowed option
      OddDepthPairOfBigraphsWithDuals(g0.addOddVertex(row1), g1.addOddVertex(row2), associativityDefects = ???)
  }
  def deleteDualPairAtOddDepth(index: Int) = OddDepthPairOfBigraphsWithDuals(g0.deleteOddVertex(index), g1.deleteOddVertex(index), associativityDefects = Memo.fail)
}

object EvenDepthPairOfBigraphsWithDuals {
  def apply(g0: EvenDepthBigraphWithDuals, g1: EvenDepthBigraphWithDuals): EvenDepthPairOfBigraphsWithDuals = {
    def defects = for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
      for (j <- 0 until g1.bigraph.rankAtDepth(-3)) yield {
        val `m*m*`: Int = {
          val updown: Int = {
            val inclusion = g0.bigraph.inclusions.last
            val duals = g0.dualData.last
            (for (k <- 0 until g0.bigraph.rankAtMaximalDepth) yield inclusion(k)(i) * inclusion(duals(k))(j)).sum
          }
          val downup: Int = {
            val inclusion = g0.bigraph.inclusions.secondLast
            val duals = g0.dualData.secondLast
            (for (k <- 0 until g0.bigraph.rankAtDepth(-3)) yield inclusion(i)(k) * inclusion(j)(duals(k))).sum
          }
          updown + downup
        }
        val `*m*m`: Int = {
          val updown: Int = {
            val inclusion = g1.bigraph.inclusions.last
            val duals = g1.dualData.last
            (for (k <- 0 until g1.bigraph.rankAtMaximalDepth) yield inclusion(k)(i) * inclusion(duals(k))(j)).sum
          }
          val downup: Int = {
            val inclusion = g1.bigraph.inclusions.secondLast
            val duals = g1.dualData.secondLast
            (for (k <- 0 until g1.bigraph.rankAtDepth(-3)) yield inclusion(i)(k) * inclusion(j)(duals(k))).sum
          }
          updown + downup
        }
        `m*m*` - `*m*m`
      }
    }
    EvenDepthPairOfBigraphsWithDuals(g0, g1, Memo(defects))
  }
}
object OddDepthPairOfBigraphsWithDuals {
  def apply(g0: OddDepthBigraphWithDuals, g1: OddDepthBigraphWithDuals): OddDepthPairOfBigraphsWithDuals = {
    def defects00 = ???
    def defects11 = ???
    OddDepthPairOfBigraphsWithDuals(g0, g1, Memo((defects00, defects11)))
  }
}

sealed trait SubfactorWeed extends CanonicalGeneration[SubfactorWeed, Seq[(Permutation, Permutation)]] { weed =>
  def indexLimit: Double
  def pair: PairOfBigraphsWithDuals

  override def findIsomorphismTo(other: SubfactorWeed) = ???
  def isomorphs = ???

  override lazy val automorphisms: FinitelyGeneratedFiniteGroup[Seq[(Permutation, Permutation)]] = ???

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
      val elements: Set[Lower] =
        decreaseDepthSet.getOrElse(
          (for (i <- 0 to 1; k <- 0 until pair(i).bigraph.rankAtMaximalDepth; if pair(i).dualData.last(k) != k - 1) yield {
            if (pair(i).dualData.last(k) == k) {
              DeleteSelfDualVertex(i, k)
            } else {
              DeleteDualPairAtEvenDepth(i, k)
            }
          }).toSet[Lower])

      def act(g: Seq[(Permutation, Permutation)], x: Lower): Lower = x match {
        case DecreaseDepth => x
        case DeleteSelfDualVertex(graph, index) => ???
        case DeleteDualPairAtEvenDepth(graph, index) => ???
      }
    }
  }
  override def ordering = ???

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
            if (duals.last == duals.size - 1 /* graph has dual pairs already */ ) {
              Seq.empty
            } else {
              // Now we have to run an odometer to produce all values of the row.
              // We certainly give this odometer the index limit.
              // What sort of limits can we give the odometer?
              // --->  Any function Seq[Int] => (L: Ordering) which is pointwise increasing and which is automorphism invariant
              // Examples:
              // * sum of the entries
              // * tally of neighbour degrees (if vertex i at the previous depth has valence n_i, we send row to (n \mapsto \sum_{i: n_i = n} row_i), ordered lexicographically
              // 
              // Of course, any limits that are used here, have to be reflected in the ordering of lowers.

              val limit = {
                row: List[Int] => pair(graph).bigraph.isEigenValueWithRowBelow_?(indexLimit)(row)
              }

              val initial = List.fill(pair(graph).bigraph.rankAtDepth(-2))(0)

              Odometer(limit)(initial).tail.map(AddSelfDualVertex(graph, _))
            }
          }

          def uppersAddingDualPairVerticesToGraph(graph: Int) = {
            ???
          }

          ((pair.g0.bigraph.rankAtMaximalDepth > 0 && pair.g1.bigraph.rankAtMaximalDepth > 0) option IncreaseDepth) ++
            uppersAddingVerticesToGraph(0) ++
            uppersAddingVerticesToGraph(1)
        }

        allUppers.filter(_.associative_?)
      }
      override def act(g: Seq[(Permutation, Permutation)], x: Upper): Upper = x match {
        case IncreaseDepth => x
        case AddSelfDualVertex(graph, row) => ???
        case AddDualPairAtEvenDepth(graph, row1, row2) => ???
      }
    }
  }

  trait AddVertexUpper extends Upper {
    def pairOption: Option[EvenDepthPairOfBigraphsWithDuals]
    def associative_? = pairOption.nonEmpty
    override lazy val result = weed.copy(pair = pairOption.get)
  }

  case class AddSelfDualVertex(graph: Int, row: Seq[Int]) extends AddVertexUpper {
    override lazy val pairOption = pair.addSelfDualVertex(graph, row)
    override def inverse = result.DeleteSelfDualVertex(graph, pair(graph).bigraph.rankAtMaximalDepth + 1)
  }
  case class AddDualPairAtEvenDepth(graph: Int, row1: Seq[Int], row2: Seq[Int]) extends AddVertexUpper {
    override lazy val pairOption = pair.addDualPairAtEvenDepth(graph, row1, row2)
    override def inverse = result.DeleteDualPairAtEvenDepth(graph, pair(graph).bigraph.rankAtMaximalDepth + 1)
  }

  case class DeleteSelfDualVertex(graph: Int, index: Int) extends Lower {
    override def result = weed.copy(pair = pair.deleteSelfDualVertex(graph, index))
  }
  case class DeleteDualPairAtEvenDepth(graph: Int, index: Int) extends Lower {
    override def result = weed.copy(pair = pair.deleteDualPairAtEvenDepth(graph, index))
  }
}

case class OddDepthSubfactorWeed(indexLimit: Double, pair: OddDepthPairOfBigraphsWithDuals) extends SubfactorWeed { weed =>

  override lazy val lowerObjects = {
    new automorphisms.Action[Lower] {
      val elements: Set[Lower] =
        decreaseDepthSet.getOrElse(
          (for (k <- 0 until pair.g0.bigraph.rankAtMaximalDepth) yield DeleteDualPairAtOddDepth(k)).toSet)

      def act(g: Seq[(Permutation, Permutation)], x: Lower): Lower = x match {
        case DecreaseDepth => x
        case DeleteDualPairAtOddDepth(index: Int) => ???
      }
    }
  }
  override def ordering = ???

  override def upperObjects = {
    new automorphisms.Action[Upper] {
      override val elements: Iterable[Upper] = {
        def uppersAddingVerticesToGraph = {
          val limit = {
            // TODO: surely we can be more efficient here; at least saving some answers
            row: List[List[Int]] => pair(0).bigraph.isEigenValueWithRowBelow_?(indexLimit)(row(0)) && pair(0).bigraph.isEigenValueWithRowBelow_?(indexLimit)(row(1))
          }
          val initial = for (i <- (0 to 1).toList) yield List.fill(pair(i).bigraph.rankAtDepth(-2))(0)
          Odometer(limit)(initial).tail.map({ rows => AddDualPairAtOddDepth(rows(0), rows(1)) })
        }

        val allUppers: Iterable[Upper] = ((pair.g0.bigraph.rankAtMaximalDepth > 0) option IncreaseDepth) ++ uppersAddingVerticesToGraph

        allUppers.filter(_.associative_?)
      }
      override def act(g: Seq[(Permutation, Permutation)], x: Upper): Upper = x match {
        case IncreaseDepth => x
        case AddDualPairAtOddDepth(row1, row2) => ???
      }
    }
  }

  trait AddVertexUpper extends Upper {
    def pairOption: Option[OddDepthPairOfBigraphsWithDuals]
    def associative_? = pairOption.nonEmpty
    override lazy val result = weed.copy(pair = pairOption.get)
  }

  case class AddDualPairAtOddDepth(row1: Seq[Int], row2: Seq[Int]) extends AddVertexUpper {
    override lazy val pairOption = pair.addDualPairAtOddDepth(row1, row2)
    override def inverse = result.DeleteDualPairAtOddDepth(pair.g0.bigraph.rankAtMaximalDepth + 1)
  }
  case class DeleteDualPairAtOddDepth(index: Int) extends Lower {
    override def result = weed.copy(pair = pair.deleteDualPairAtOddDepth(index))
  }

}