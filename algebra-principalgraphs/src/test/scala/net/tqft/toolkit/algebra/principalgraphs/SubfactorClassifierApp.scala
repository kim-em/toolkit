package net.tqft.toolkit.algebra.principalgraphs

import scala.IndexedSeq

object SubfactorClassifierApp extends App {
	val A2 = SubfactorWeed(4.1, PairOfBigraphsWithDuals(BigraphWithDuals("bwd1vduals1"), BigraphWithDuals("bwd1vduals1")))
	require(Bigraph("gbg1v").depth == 2)
	require(BigraphWithDuals("bwd1vduals1").bigraph.depth == 2)
	require(A2.pair.depth == 2)
	
	def p(w: SubfactorWeed) = {
	  println(w)
	  println(w.pair(0).bigraph.estimateEigenvalue(10))
	  println(w.pair(1).bigraph.estimateEigenvalue(10))
	}

	for(d <- A2.descendantsWithSupertransitivityAtMost(2); if d.pair.g0.bigraph.rankAtMaximalDepth == 0 && d.pair.g1.bigraph.rankAtMaximalDepth == 0) {
	  p(d)
	}
}