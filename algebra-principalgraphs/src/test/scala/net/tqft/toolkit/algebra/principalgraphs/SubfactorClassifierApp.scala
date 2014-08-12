package net.tqft.toolkit.algebra.principalgraphs

import scala.IndexedSeq

object SubfactorClassifierApp extends App {
	val A3 = SubfactorWeed(4.4, PairOfBigraphsWithDuals(BigraphWithDuals("bwd1v1vduals1v1"), BigraphWithDuals("bwd1v1vduals1v1")))
	require(A3.pair.depth == 3)
	
	def p(w: SubfactorWeed) = {
	  println(w)
	  println(w.pair(0).bigraph.estimateEigenvalue(10))
	  println(w.pair(1).bigraph.estimateEigenvalue(10))
	}

	for(d <- A3.descendantsWithSupertransitivityAtMost(3)) {
	  if(d.pair.g0.bigraph.rankAtMaximalDepth == 0 && d.pair.g1.bigraph.rankAtMaximalDepth == 0) {
		p(d.DecreaseDepth.result)
	  } else if(d.upperObjects.elements.isEmpty) {
	    p(d)
	  }
	}
}