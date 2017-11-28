package net.tqft.toolkit.collections

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IteratorsParallelCombineTest extends FlatSpec with Matchers {

  "parallelCombine" should "work correctly" in {
    import Iterators._

    val m = 100
    val n = 10
    
    parallelCombine(Seq.tabulate(n)({ i => ((i*m) until (i+1)*m).iterator })).toSeq.sorted should equal ((0 until n*m))
  }
}

