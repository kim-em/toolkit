package net.tqft.toolkit.algebra.principalgraphs

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.math._

@RunWith(classOf[JUnitRunner])
class PairOfBigraphsWithDualsTest extends FlatSpec with Matchers {

  "passesTriplePointObstruction_?" should "allow Haagerup, but not `self-dual` Haagerup" in {
    //     println(PairOfBigraphsWithDuals.Examples.Haagerup.updatedVerticesByDimension) 
    PairOfBigraphsWithDuals.Examples.Haagerup.passesTriplePointObstruction_? should equal(true)

    val selfDualHaagerup = PairOfBigraphsWithDuals(BigraphWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v1x2"), BigraphWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v1x2"))
    //    println(selfDualHaagerup.updatedVerticesByDimension)
    selfDualHaagerup.triplePointConfigurations.value.size should equal(1)
    selfDualHaagerup.passesTriplePointObstruction_? should equal(false)
  }

}