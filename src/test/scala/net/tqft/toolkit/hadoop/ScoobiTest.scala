package net.tqft.toolkit.hadoop

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.nicta.scoobi.Scoobi._
import ScoobiHelper._

@RunWith(classOf[JUnitRunner])
class ScoobiTest extends FlatSpec with ShouldMatchers {

  "Scoobi" should "make me happy" in {
    DList((3, 0), (9, 2)).map(_._1).hither.toList should equal(List(3,9))
  }
  
  "Scoobi" should "make me happy (2)" in {
    val s = DList(1,2,3)
    val f = s.map(_ * 3).filter(_ % 2 != 0)
    s.map(x => x).hither.toList should equal(List(1,2,3))
    f.hither.toList.reduce(_ * _) should equal(27)
  }
  "Scoobi" should "make me happy (3)" in {
    val s = DList((1,0),(2,1),(3,2))
    s.filter(p => p._1 % 2 != 0).map(_._2).hither.toList should equal(List(0,2))
  }

}
