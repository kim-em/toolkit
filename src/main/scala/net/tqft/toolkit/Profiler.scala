package net.tqft.toolkit

trait Profiler {
	  def timing[R](f: => R): (Long, R) = {
	 	  val startTime = System.nanoTime
	 	  val result = f
	 	  ((System.nanoTime - startTime)/1000000, result)
	  }

	  import collection.generic.CanBuildFrom
	  import collection.TraversableLike
	  def partialSums[CC[X] <: TraversableLike[X, CC[X]], A](traversable: CC[A])(implicit m: Monoid[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
	 	  traversable.scanLeft(m.unit)({ m.add(_, _) })
	  }
	  
	  import collection.IterableLike
	  def movingAverage[CC[X] <: IterableLike[X, CC[X]]](period: Int)(iterable: CC[Long]): Iterator[Long] = {
	 	  iterable sliding(period) map { _ sum } map { _ / period }
	  }
	   
	  def successiveTimingAverages[R](f: => R): Stream[Long] = {
	 	  implicit val m = Monoids.longMonoid
	 	  partialSums(Stream.continually(timing(f)._1)).zipWithIndex.tail map { case (t,n) => t/n }
	  }
	   
	  def movingTimingAverages[R](period: Int)(f: => R): TraversableOnce[Long] = {
	 	  movingAverage(period)(Stream.continually(timing(f)._1))
	  }
}
object Profiler extends Profiler

abstract class Monoid[T] {
	def add(x: T, y: T): T
	def unit: T
}
object Monoids {
	implicit object stringMonoid extends Monoid[String] {
		def add(x: String, y: String): String = x.concat(y)
		def unit: String = ""
	}
	implicit object intMonoid extends Monoid[Int] {
		def add(x: Int, y: Int): Int = x + y
		def unit: Int = 0
	}
	implicit object longMonoid extends Monoid[Long] {
		def add(x: Long, y: Long): Long = x + y
		def unit: Long = 0
	}
}