package net.tqft.toolkit.algebra.principalgraphs

object SubfactorClassifierApp extends App {
	val A1 = OddDepthSubfactorWeed(4.0, OddDepthPairOfBigraphsWithDuals(OddDepthBigraphWithDuals(Bigraph(1, Seq(Seq.empty)), Seq(IndexedSeq(0))), OddDepthBigraphWithDuals(Bigraph(1, Seq(Seq.empty)), Seq(IndexedSeq(0)))))
	val A2 = EvenDepthSubfactorWeed(4.4, EvenDepthPairOfBigraphsWithDuals(EvenDepthBigraphWithDuals(Bigraph(1, Seq(Seq(Seq(1)), Seq.empty)), Seq(IndexedSeq(0), IndexedSeq())), EvenDepthBigraphWithDuals(Bigraph(1, Seq(Seq(Seq(1)), Seq.empty)), Seq(IndexedSeq(0), IndexedSeq()))))
	def p(w: SubfactorWeed) = {
	  println(w)
	  println(w.pair(0).bigraph.estimateEigenvalue(10))
	  println(w.pair(0).bigraph.estimateEigenvalue(10))
	}
//	for(d1 <- A1.children) {
//	  p(d1)
//	  for(d2 <- d1.children) {
//	    p(d2)
//	    for(d3 <- d2.children) {
//	      p(d3)
//	    }
//	  }
//	}
	for(d <- A2.descendantsWithSupertransitivityAtMost(2); if d.pair.g0.bigraph.rankAtMaximalDepth == 0 && d.pair.g1.bigraph.rankAtMaximalDepth == 0) {
	  p(d.DecreaseDepth.result)
	}
}