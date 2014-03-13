package net.tqft.toolkit.algebra.principalgraphs

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.permutations.Permutation
import net.tqft.toolkit.permutations.Involutions
import net.tqft.toolkit.permutations.Involutions._
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import scalaz._
import Scalaz._

case class Bigraph(rankAtDepthZero: Int, inclusions: Seq[Seq[Seq[Int]]]) {
  def addRow(row: Seq[Int]) = copy(inclusions = inclusions.most :+ (inclusions.last :+ row))
  def deleteRow(index: Int) = copy(inclusions = inclusions.most :+ (inclusions.last.removed(index)))
  def rankAtMaximalDepth: Int = rankAtDepth(depth)
  def rankAtDepth(k: Int): Int = {
    k match {
      case 0 => rankAtDepthZero
      case k if k < 0 => rankAtDepth(depth + 1 + k)
      case k if k > 0 => inclusions(k - 1).size
    }
  }
  def depth: Int = inclusions.size

  def truncate: Bigraph = Bigraph(rankAtDepthZero, inclusions.most)
  def increaseDepth: Bigraph = Bigraph(rankAtDepthZero, inclusions :+ Seq.empty)
}

trait BigraphWithDuals {
  def bigraph: Bigraph
  def dualData: Seq[Involution]
  
  def truncate: BigraphWithDuals
  def increaseDepth: BigraphWithDuals
}

case class EvenDepthBigraphWithDuals(bigraph: Bigraph, dualData: Seq[Involution]) extends BigraphWithDuals {
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

case class EvenDepthPairOfBigraphsWithDuals(g0: EvenDepthBigraphWithDuals, g1: EvenDepthBigraphWithDuals, associativityDefects: Option[Seq[Seq[Int]]]) extends PairOfBigraphsWithDuals {

  override def apply(i: Int): EvenDepthBigraphWithDuals = i match {
    case 0 => g0
    case 1 => g1
  }

  override def associativeAtPenultimateDepth_? = {
    require(associativityDefects.nonEmpty)
    defectsZero_?(associativityDefects.get)
  }

  override def truncate = OddDepthPairOfBigraphsWithDuals(g0.truncate, g1.truncate, None)
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
    case 0 => copy(g0 = g0.deleteSelfDualVertex(index), associativityDefects = None)
    case 1 => copy(g1 = g1.deleteSelfDualVertex(index), associativityDefects = None)
  }
  def deleteDualPairAtEvenDepth(graph: Int, index: Int): EvenDepthPairOfBigraphsWithDuals = graph match {
    case 0 => copy(g0 = g0.deleteDualPairOfVertices(index), associativityDefects = None)
    case 1 => copy(g1 = g1.deleteDualPairOfVertices(index), associativityDefects = None)
  }
}

case class OddDepthPairOfBigraphsWithDuals(g0: OddDepthBigraphWithDuals, g1: OddDepthBigraphWithDuals, associativityDefects: Option[(Seq[Seq[Int]], Seq[Seq[Int]])]) extends PairOfBigraphsWithDuals {
  override def apply(i: Int): OddDepthBigraphWithDuals = i match {
    case 0 => g0
    case 1 => g1
  }

  override def associativeAtPenultimateDepth_? = {
    require(associativityDefects.nonEmpty)
    defectsZero_?(associativityDefects.get._1) && defectsZero_?(associativityDefects.get._2)
  }

  override def truncate = EvenDepthPairOfBigraphsWithDuals(g0.truncate, g1.truncate, None)
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
  def deleteDualPairAtOddDepth(index: Int) = OddDepthPairOfBigraphsWithDuals(g0.deleteOddVertex(index), g1.deleteOddVertex(index), associativityDefects = None)
}

object EvenDepthPairOfBigraphsWithDuals {
  def apply(g0: EvenDepthBigraphWithDuals, g1: EvenDepthBigraphWithDuals): EvenDepthPairOfBigraphsWithDuals = {
    val defects = for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
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
    EvenDepthPairOfBigraphsWithDuals(g0, g1, Some(defects))
  }
}
object OddDepthPairOfBigraphsWithDuals {
  def apply(g0: OddDepthBigraphWithDuals, g1: OddDepthBigraphWithDuals): OddDepthPairOfBigraphsWithDuals = {
    val defects00 = ???
    val defects11 = ???
    OddDepthPairOfBigraphsWithDuals(g0, g1, Some((defects00, defects11)))
  }
}

sealed trait SubfactorWeed extends CanonicalGeneration[SubfactorWeed, Seq[(Permutation, Permutation)]] { weed =>
  def pair: PairOfBigraphsWithDuals

  override def findIsomorphismTo(other: SubfactorWeed) = ???
  def isomorphs = ???

  override val automorphisms: FinitelyGeneratedFiniteGroup[Seq[(Permutation, Permutation)]] = ???

  sealed trait Upper {
    val result: SubfactorWeed
    def inverse: result.Lower

    def associative_? : Boolean
  }
  case object IncreaseDepth extends Upper {
    override lazy val result =  SubfactorWeed(pair.increaseDepth)
    override def inverse = result.DecreaseDepth

    override def associative_? = pair.associativeAtPenultimateDepth_?
  }

  def decreaseDepthSet: Option[Set[Lower]] = (pair.g0.bigraph.rankAtMaximalDepth == 0 && pair.g1.bigraph.rankAtMaximalDepth == 0) option Set(DecreaseDepth)

  sealed trait Lower {
    def result: SubfactorWeed
  }
  case object DecreaseDepth extends Lower {
    override def result = SubfactorWeed(pair.truncate)
  }
}

object SubfactorWeed {
  def apply(pair: PairOfBigraphsWithDuals) = {
    pair match {
      case pair: EvenDepthPairOfBigraphsWithDuals => EvenDepthSubfactorWeed(pair)
      case pair: OddDepthPairOfBigraphsWithDuals => OddDepthSubfactorWeed(pair)
    }
  }
}

case class EvenDepthSubfactorWeed(pair: EvenDepthPairOfBigraphsWithDuals) extends SubfactorWeed { weed =>

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

      def act(g: Seq[(Permutation, Permutation)], x: Lower): Lower = ???
    }
  }
  override val ordering = ???

  override def upperObjects = {
    new automorphisms.Action[Upper] {
      val elements: Set[Upper] = {
        val allUppers: Set[Upper] = ??? // TODO remember that in this case associativityDefects are non-decreasing, so anything with a positive associativityDefect can be rejected!

        allUppers.filter(_.associative_?)
      }
      def act(g: Seq[(Permutation, Permutation)], x: Upper): Upper = ???
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

case class OddDepthSubfactorWeed(pair: OddDepthPairOfBigraphsWithDuals) extends SubfactorWeed { weed =>

  override lazy val lowerObjects = {
    new automorphisms.Action[Lower] {
      val elements: Set[Lower] =
        decreaseDepthSet.getOrElse(
          (for (k <- 0 until pair.g0.bigraph.rankAtMaximalDepth) yield DeleteDualPairAtOddDepth(k)).toSet)

      def act(g: Seq[(Permutation, Permutation)], x: Lower): Lower = ???
    }
  }
  override val ordering = ???

  override def upperObjects = {
    new automorphisms.Action[Upper] {
      val elements: Set[Upper] = {
        val allUppers: Set[Upper] = ???

        allUppers.filter(_.associative_?)
      }
      def act(g: Seq[(Permutation, Permutation)], x: Upper): Upper = ???
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