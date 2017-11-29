package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.spiders._

object EnumerationContexts {

  val spider = implicitly[DiagramSpider[PlanarGraph]]

  def tetravalentGraphsWithNoSmallFaces(maxFaces: Int, maxBoundaryPoints: Int) = {
    val root = PlanarGraph.star(4)
    val bigon = spider.multiply(root, root, 2)
    val triangle = spider.multiply(spider.rotate(spider.multiply(root, root, 1), 2), root, 2)
    PlanarGraphEnumerationContext(Seq(VertexType(4, 0, 1)), Seq(bigon, triangle), Seq(root), maxFaces, maxBoundaryPoints)
  }
}