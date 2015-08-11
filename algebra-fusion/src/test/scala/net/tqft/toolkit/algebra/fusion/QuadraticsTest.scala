package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class QuadraticsTest extends FlatSpec with Matchers {

  "zero_?" should "report whether a quadratic is zero" in {
    Quadratic[String](LinearTerm(0, Map.empty), Seq.empty).zero_? should be (true)
  }
}