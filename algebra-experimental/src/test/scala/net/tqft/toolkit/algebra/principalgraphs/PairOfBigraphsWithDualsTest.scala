package net.tqft.toolkit.algebra.principalgraphs

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.math._

@RunWith(classOf[JUnitRunner])
class PairOfBigraphsWithDualsTest extends FlatSpec with Matchers {

  "passesTriplePointObstruction_?" should "allow Haagerup, but not `self-dual` Haagerup" in {
    PairOfBigraphsWithDuals.Examples.Haagerup.passesTriplePointObstruction_? should equal(true)
    PairOfBigraphsWithDuals(BigraphWithDuals.Examples.Haagerup, BigraphWithDuals.Examples.Haagerup).passesTriplePointObstruction_? should equal(false)
  }
  
}