package net.tqft.toolkit.algebra.spiders.sandbox

import net.tqft.toolkit.algebra.spiders._

object DrawPlanarGraph {
	val G = Plantri(7,5)(2)
                                                  
  val vertexAdjList = for (v <- 0 until G.numberOfVertices) yield G.neighboursOf(v)
	vertexAdjList.map(println(_))
}