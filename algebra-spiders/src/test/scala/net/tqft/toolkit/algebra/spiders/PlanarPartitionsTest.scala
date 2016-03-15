package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import net.tqft.toolkit.Profiler

@RunWith(classOf[JUnitRunner])
class PlanarPartitionsTest extends FlatSpec with Matchers with IsomorphismMatchers {
  val numberOfPlanarPartitions = Seq(1, 0, 1, 1, 3, 6, 15, 36, 91, 232, 603)
  
  for((n, k) <- numberOfPlanarPartitions.zipWithIndex) {
    PlanarPartitions(k).size should equal(n)
  }
  
  println(Profiler.timing(PlanarPartitions(16).size))
}

