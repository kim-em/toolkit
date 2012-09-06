package net.tqft.toolkit

trait Profiler {
	  def timing[R](f: => R): (Long, R) = {
	 	  val startTime = System.nanoTime
	 	  val result = f
	 	  ((System.nanoTime - startTime)/1000000, result)
	  }

	  def partialSums(traversable: Iterable[Long]): Iterable[Long] = {
	 	  traversable.scanLeft(0L)({ _ + _ })
	  }
	  
	  import collection.IterableLike
	  def movingAverage(period: Int)(iterable: Iterable[Long]): Iterable[Long] = {
	 	  iterable.sliding(period).map(_.sum).map(_ / period).toIterable
	  }
	   
	  def successiveTimingAverages[R](f: => R): Iterable[Long] = {
	 	  partialSums(Stream.continually(timing(f)._1)).zipWithIndex.tail.map({ case (t,n) => t/n })
	  }
	   
	  def movingTimingAverages[R](period: Int)(f: => R): Iterable[Long] = {
	 	  movingAverage(period)(Stream.continually(timing(f)._1))
	  }
}
object Profiler extends Profiler
