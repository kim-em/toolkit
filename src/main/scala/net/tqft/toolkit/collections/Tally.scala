package net.tqft.toolkit.collections

object Tally {

	implicit def tallyable[A](i: Iterable[A]) = new Tallyable(i)
	
	class Tallyable[A](i: Iterable[A]) {
		def tally = {
			i.groupBy(x => x) mapValues (_.size) toList
		}
	}
	
}