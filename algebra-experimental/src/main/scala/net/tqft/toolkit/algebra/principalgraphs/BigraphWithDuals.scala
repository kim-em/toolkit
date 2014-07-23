package net.tqft.toolkit.algebra.principalgraphs

import net.tqft.toolkit.permutations.Permutations._
import net.tqft.toolkit.permutations.Involutions
import net.tqft.toolkit.permutations.Involutions._

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
