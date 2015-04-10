package net.tqft.toolkit.algebra.spiders.sandbox

import net.tqft.toolkit.algebra.spiders._

object DrawPlanarGraph {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(142); 
	val G = Plantri(7,5)(2);System.out.println("""G  : net.tqft.toolkit.algebra.spiders.PlanarGraph = """ + $show(G ));$skip(135); 
                                                  
  val vertexAdjList = for (v <- 0 until G.numberOfVertices) yield G.neighboursOf(v);System.out.println("""vertexAdjList  : scala.collection.immutable.IndexedSeq[Seq[Int]] = """ + $show(vertexAdjList ));$skip(31); val res$0 = 
	vertexAdjList.map(println(_));System.out.println("""res0: scala.collection.immutable.IndexedSeq[Unit] = """ + $show(res$0))}
}
