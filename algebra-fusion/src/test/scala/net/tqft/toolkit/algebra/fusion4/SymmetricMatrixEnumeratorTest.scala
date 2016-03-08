package net.tqft.toolkit.algebra.fusion4

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

@RunWith(classOf[JUnitRunner])
class SymmetricMatrixEnumeratorTest extends FlatSpec with Matchers {

  "SymmetricMatrixEnumerator" should "find all 3x3 symmetric matrices with eigenvalue < 3.0" in {
    val enumeration = SymmetricMatrixEnumerator(Array.fill(3,3)(0), 3.0, (for(i <- 0 until 3; j <- i until 3) yield Set((i,j),(j,i)).toList).toArray)
    enumeration.size should equal(351)
  }

}