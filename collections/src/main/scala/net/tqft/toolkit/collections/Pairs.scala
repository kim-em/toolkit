package net.tqft.toolkit.collections

import scala.language.higherKinds
import scala.collection.IterableLike
import scala.collection.GenIterable

object Pairs {
	
  // TODO shapeless takes care of this stuff better
	implicit class UniformPair[A](pair: Pair[A,A]) {
		def map[B](f: A => B): Pair[B, B] = (f(pair._1), f(pair._2))
		def toList: List[A] = List(pair._1, pair._2)
		def toSeq: Seq[A] = Seq(pair._1, pair._2)
		def toSet: Set[A] = Set(pair._1, pair._2)
	}
	
	
	import collection.generic.CanBuildFrom
	import collection.TraversableLike

	implicit class TransposableTraversableOfPairs[CC[X] <: TraversableLike[X, CC[X]], A, B](traversable: CC[(A, B)]) {
		def transpose
		    (implicit cbf1: CanBuildFrom[CC[(A,B)], A, CC[A]], 
		              cbf2: CanBuildFrom[CC[(A,B)], B, CC[B]]): (CC[A], CC[B]) = 
			{
				(traversable map { _._1 }, traversable map { _._2 })
			}
	}

	implicit class TransposablePairOfTraversables[CC[X] <: IterableLike[X, CC[X]], A, B](p: (CC[A], GenIterable[B])) {
	  def transpose(implicit bf: CanBuildFrom[CC[A], (A, B), CC[(A, B)]]): CC[(A, B)] = 
	    p._1.zip[A, B, CC[(A, B)]](p._2)
	}
	
}