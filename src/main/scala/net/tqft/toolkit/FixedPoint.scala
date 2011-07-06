package net.tqft.toolkit

object FixedPoint {

	implicit def fixedPointForFunction[A](f: A => A) = new FunctionFixedPoint(f)
	
	class FunctionFixedPoint[A](f: A => A) {
		def fixedPoint(a: A): A = {
			val iterates = Stream.iterate(a)(f)
			((iterates zip iterates.tail) find { case (a,b) => a == b }).get._1 
		}
	}
	
}