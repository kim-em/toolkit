package net.tqft.toolkit.algebra.principalgraphs

import net.tqft.toolkit.permutations.Permutations._
import net.tqft.toolkit.permutations.Involutions
import net.tqft.toolkit.permutations.Involutions._

trait BigraphWithDuals {
  def bigraph: Bigraph
  def dualData: Seq[Involution]

  def supertransitivity = bigraph.supertransitivity
  
  override def toString = "bwd" + bigraph.toString.stripPrefix("gbg") + "duals" + dualData.map(_.map(_ + 1).mkString("x")).mkString("v")

  def truncate: BigraphWithDuals
  def increaseDepth: BigraphWithDuals
}

object BigraphWithDuals {
  def apply(string: String): BigraphWithDuals = {
    require(string.startsWith("bwd"))
    require(string.contains("duals"))
    val Seq(gbg, duals) = string.split_!("duals")
    val bigraph = Bigraph("gbg" + gbg.stripPrefix("bwd"))
    val dualData = duals.split_!("v").map(_.split_!("x").map(_.toInt - 1).toIndexedSeq)
    bigraph.depth % 2 match {
      case 0 => EvenDepthBigraphWithDuals(bigraph, dualData)
      case 1 => OddDepthBigraphWithDuals(bigraph, dualData)
    }
  }

  object Examples {
    val Haagerup = apply("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v2x1")
    val dualHaagerup = apply("bwd1v1v1v1p1v1x0p1x0duals1v1v1x2")
  }
}

case class EvenDepthBigraphWithDuals(bigraph: Bigraph, dualData: Seq[Involution]) extends BigraphWithDuals {
  require(bigraph.depth % 2 == 0)
  require(bigraph.depth / 2 + 1 == dualData.size)

  def numberOfSelfDualObjectsAtMaximalDepth = dualData.last.zipWithIndex.count(p => p._1 == p._2)
  
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

  // this checks if a row can be added to the other graph,
  // satisfying the associativity condition for paths from vertices at depth n-2 on _this_ graph to the new vertex on the _other_ graph
  def rowAllowed_?(row: Seq[Int]): Boolean = {
    (for (i <- (0 until bigraph.rankAtDepth(-3)).iterator) yield {
      (for ((r, j) <- bigraph.inclusions.secondLast.zipWithIndex) yield (r(i) - r(dualData.secondLast(i))) * row(j)).sum
    }).forall(_ == 0)
  }
  // this checks if a pair of rows can be added to the other graph,
  // satisfying the associativity condition for paths from vertices at depth n-2 on _this_ graph to the new vertices on the _other_ graph
  def rowsAllowed_?(row0: Seq[Int], row1: Seq[Int]): Boolean = {
    (for (i <- (0 until bigraph.rankAtDepth(-3)).iterator) yield {
      var s = 0
      var t = 0
      for ((r, j) <- bigraph.inclusions.secondLast.zipWithIndex) {
        s += r(i) * row1(j) - r(dualData.secondLast(i)) * row0(j)
        t += r(i) * row0(j) - r(dualData.secondLast(i)) * row1(j)
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
