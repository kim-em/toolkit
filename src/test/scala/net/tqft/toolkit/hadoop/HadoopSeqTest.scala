package net.tqft.toolkit.hadoop

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HadoopSeqTest extends FlatSpec with ShouldMatchers {

    "hadoopiness" should "make me happy" in {
  	  val s = HadoopSeq(1,2,3)
  	  s.size should equal(3)
  	  s.toString should equal("HadoopSeq(1, 2, 3)")
    }

  "isEmpty" should "be false after a filter that leaves some elements" in {
    val s = HadoopSeq(1, 2, 3)
    s.filter(_ % 2 != 0).isEmpty should equal(false)
  }

  "reduce" should "run in Hadoop" in {
    val s = HadoopSeq(1, 2, 3)
    s.map(_ * 3).filter(_ % 2 != 0).reduce(_ * _) should equal(27)
  }
  
  

}
