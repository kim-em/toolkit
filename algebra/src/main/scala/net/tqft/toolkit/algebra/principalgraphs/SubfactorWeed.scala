package net.tqft.toolkit.algebra.principalgraphs

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.permutations.Permutation
import net.tqft.toolkit.permutations.Involutions
import net.tqft.toolkit.permutations.Involutions._
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup

case class Bigraph(rankAtDepthZero: Int, inclusions: Seq[Seq[Seq[Int]]]) {
  def addRow(row: Seq[Int]) = copy(inclusions = inclusions.most :+ (inclusions.last :+ row))
  def deleteRow(index: Int) = copy(inclusions = inclusions.most :+ (inclusions.last.removed(index)))
  def rankAtMaximalDepth: Int = rankAtDepth(depth)
  def rankAtDepth(k: Int): Int = {
    k match {
      case 0 => rankAtDepthZero
      case k => inclusions(k - 1).size
    }
  }
  def depth: Int = inclusions.size
}

case class BigraphWithDuals(bigraph: Bigraph, dualData: Seq[Involution]) {
  def addSelfDualVertex(row: Seq[Int]): BigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    BigraphWithDuals(bigraph.addRow(row), dualData.most :+ (dualData.last :+ (bigraph.rankAtMaximalDepth + 1)))
  }
  def deleteSelfDualVertex(index: Int): BigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    require(dualData.last(index) == index)
    BigraphWithDuals(bigraph.deleteRow(index), dualData.most :+ dualData.last.collect({ case i if i < index => i; case i if i > index => i - 1 }))
  }
  def addDualPairOfVertices(row1: Seq[Int], row2: Seq[Int]): BigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    BigraphWithDuals(bigraph.addRow(row1).addRow(row2), dualData.most :+ (dualData.last :+ (bigraph.rankAtMaximalDepth + 2) :+ (bigraph.rankAtMaximalDepth + 1)))
  }
  def deleteDualPairOfVertices(index: Int): BigraphWithDuals = {
    require(bigraph.depth % 2 == 0)
    require(dualData.last(index) == index + 1)
    BigraphWithDuals(bigraph.deleteRow(index).deleteRow(index), dualData.most :+ dualData.last.collect({ case i if i < index => i; case i if i > index + 1 => i - 2 }))
  }
  def addOddVertex(row: Seq[Int]) = copy(bigraph.addRow(row))
  def deleteOddVertex(index: Int) = copy(bigraph.deleteRow(index))
}

case class PairOfBigraphsWithDuals(g0: BigraphWithDuals, g1: BigraphWithDuals) {
  def apply(i: Int) = i match {
    case 0 => g0
    case 1 => g1
  }
}

case class SubfactorWeed(depth: Int, pair: PairOfBigraphsWithDuals) extends CanonicalGeneration[SubfactorWeed, Seq[(Permutation, Permutation)]] { weed =>
  override def findIsomorphismTo(other: SubfactorWeed) = ???
  def isomorphs = ???

  override val automorphisms: FinitelyGeneratedFiniteGroup[Seq[(Permutation, Permutation)]] = ???

  override def lowerObjects = {
    new automorphisms.Action[Lower] {
      val elements =
        ((if (pair.g0.bigraph.rankAtMaximalDepth == 0 && pair.g1.bigraph.rankAtMaximalDepth == 0) {
          Seq(DecreaseDepth)
        } else {
          Seq.empty[Lower]
        }) ++
          (if (depth % 2 == 0) {
            for (i <- 0 to 1; k <- 0 until pair(i).bigraph.rankAtMaximalDepth; if pair(i).dualData.last(k) != k - 1) yield {
              if (pair(i).dualData.last(k) == k) {
                DeleteSelfDualVertex(i, k)
              } else {
                DeleteDualPairAtEvenDepth(i, k)
              }
            }
          } else {
            for (k <- 0 until pair.g0.bigraph.rankAtMaximalDepth) yield DeleteDualPairAtOddDepth(k)
          })).toSet

      def act(g: Seq[(Permutation, Permutation)], x: Lower): Lower = ???
    }
  }
  override val ordering = ???

  override def upperObjects = ???

  sealed trait Upper {
    val result: SubfactorWeed
    def inverse: result.Lower
  }
  case object IncreaseDepth extends Upper {
    override lazy val result = weed.copy(depth = depth + 1)
    override def inverse = result.DecreaseDepth
  }
  case class AddSelfDualVertex(graph: Int, row: Seq[Int]) extends Upper {
    override lazy val result = {
      weed.copy(pair = graph match {
        case 0 => pair.copy(g0 = pair(graph).addSelfDualVertex(row))
        case 1 => pair.copy(g1 = pair(graph).addSelfDualVertex(row))
      })
    }
    override def inverse = result.DeleteSelfDualVertex(graph, pair(graph).bigraph.rankAtMaximalDepth + 1)
  }
  case class AddDualPairAtEvenDepth(graph: Int, row1: Seq[Int], row2: Seq[Int]) extends Upper {
    override lazy val result = {
      weed.copy(pair = graph match {
        case 0 => pair.copy(g0 = pair(graph).addDualPairOfVertices(row1, row2))
        case 1 => pair.copy(g1 = pair(graph).addDualPairOfVertices(row1, row2))
      })
    }
    override def inverse = result.DeleteDualPairAtEvenDepth(graph, pair(graph).bigraph.rankAtMaximalDepth + 1)
  }
  case class AddDualPairAtOddDepth(row1: Seq[Int], row2: Seq[Int]) extends Upper {
    override lazy val result = weed.copy(pair = PairOfBigraphsWithDuals(pair.g0.addOddVertex(row1), pair.g1.addOddVertex(row2)))
    override def inverse = result.DeleteDualPairAtOddDepth(pair.g0.bigraph.rankAtMaximalDepth + 1)
  }

  sealed trait Lower {
    def result: SubfactorWeed
  }
  case object DecreaseDepth extends Lower {
    override def result = weed.copy(depth = depth - 1)
  }
  case class DeleteSelfDualVertex(graph: Int, index: Int) extends Lower {
    override def result = weed.copy(pair = graph match {
      case 0 => pair.copy(g0 = pair(graph).deleteSelfDualVertex(index))
      case 1 => pair.copy(g1 = pair(graph).deleteSelfDualVertex(index))
    })
  }
  case class DeleteDualPairAtEvenDepth(graph: Int, index: Int) extends Lower {
    override def result = weed.copy(pair = graph match {
      case 0 => pair.copy(g0 = pair(graph).deleteDualPairOfVertices(index))
      case 1 => pair.copy(g1 = pair(graph).deleteDualPairOfVertices(index))
    })
  }
  case class DeleteDualPairAtOddDepth(index: Int) extends Lower {
    override def result = weed.copy(pair = PairOfBigraphsWithDuals(pair.g0.deleteOddVertex(index), pair.g1.deleteOddVertex(index)))
  }
}