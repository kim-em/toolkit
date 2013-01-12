package net.tqft.toolkit.collections

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class SparseSeqTest extends FlatSpec with ShouldMatchers {
  
  "toSeq" should "the original Seq" in {
    import SparseSeq._
    val seq = Seq(1,0,2,3,0)
    val sseq: SparseSeq[Int] = seq
    sseq.toSeq should equal (seq)
  }
  
}

