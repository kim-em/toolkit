package net.tqft.toolkit.algebra.principalgraphs

object SubfactorClassifierApp extends App {
	val A1 = OddDepthSubfactorWeed(6.0, OddDepthPairOfBigraphsWithDuals(OddDepthBigraphWithDuals(Bigraph(1, Seq(Seq.empty)), Seq(IndexedSeq(0))), OddDepthBigraphWithDuals(Bigraph(1, Seq(Seq.empty)), Seq(IndexedSeq(0)))))
	for(d <- A1.descendants()) {
	  println(d)
	}
}