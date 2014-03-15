package net.tqft.toolkit.algebra.principalgraphs

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.permutations.Permutations._
import net.tqft.toolkit.permutations.Involutions
import net.tqft.toolkit.permutations.Involutions._
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup

case class FusionGraph(
  rankAtDepthZero: Int,
  to: Seq[Seq[Seq[Int]]],
  amongst: Seq[Seq[Seq[Int]]],
  from: Seq[Seq[Seq[Int]]],
  dualData: Seq[Permutation]) {
  def addSelfDualVertex(fromBelow: Seq[Int], toBelow: Seq[Int], fromSame: Seq[Int], toSame: Seq[Int], selfLoops: Int) = {
    copy(
      to = to.most :+ (to.last :+ fromBelow),
      amongst = amongst.most :+ ???,
      from = from.most :+ (from.last :+ toBelow),
      dualData = dualData.most :+ (dualData.last :+ (rankAtMaximalDepth + 1))
      )
  }
  def addDualPair() = {
   
  }
  def deleteSelfDualVertex(index: Int) = ???
  def deleteDualPair(index: Int) = ???
  def rankAtMaximalDepth: Int = rankAtDepth(depth)
  def rankAtDepth(k: Int): Int = {
    k match {
      case 0 => rankAtDepthZero
      case k => to(k - 1).size
    }
  }
  def depth: Int = to.size
}

case class FusionWeed(depth: Int, graph: FusionGraph) extends CanonicalGeneration[FusionWeed, Seq[Permutation]] { weed =>
  override def findIsomorphismTo(other: FusionWeed) = ???
  def isomorphs = ???

  override val automorphisms: FinitelyGeneratedFiniteGroup[Seq[Permutation]] = ???

  override lazy val lowerObjects = {
    new automorphisms.Action[Lower] {
      val elements = ???

      def act(g: Seq[Permutation], x: Lower): Lower = ???
    }
  }
  override val ordering = ???

  override def upperObjects = ???

  sealed trait Upper {
    val result: FusionWeed
    def inverse: result.Lower
  }
  case object IncreaseDepth extends Upper {
    override lazy val result = weed.copy(depth = depth + 1)
    override def inverse = ???
  }
  case class AddSelfDualVertex() extends Upper {
    override lazy val result = weed.copy(graph = ???)
    override def inverse = ???
  }
  case class AddDualPair() extends Upper {
    override lazy val result = ???
    override def inverse = ???
  }

  sealed trait Lower {
    def result: FusionWeed
  }
  case object DecreaseDepth extends Lower {
    override def result = weed.copy(depth = depth - 1)
  }
}