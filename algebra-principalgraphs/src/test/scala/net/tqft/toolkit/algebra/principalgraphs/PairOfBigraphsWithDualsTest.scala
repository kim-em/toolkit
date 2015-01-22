package net.tqft.toolkit.algebra.principalgraphs

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.math._

@RunWith(classOf[JUnitRunner])
class PairOfBigraphsWithDualsTest extends FlatSpec with Matchers {

  "" should "" in {
    PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0vduals1v1v1x2v", "bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v1x2")
  }
  
  "passesTriplePointObstruction_?" should "allow Haagerup, but not `self-dual` Haagerup" in {
    //     println(PairOfBigraphsWithDuals.Examples.Haagerup.updatedVerticesByDimension) 
    PairOfBigraphsWithDuals.Examples.Haagerup.passesTriplePointObstruction_? should equal(true)

    val selfDualHaagerup = PairOfBigraphsWithDuals(BigraphWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v1x2"), BigraphWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v1x2"))
    //    println(selfDualHaagerup.updatedVerticesByDimension)
    selfDualHaagerup.triplePointConfigurations.value.size should equal(1)
    selfDualHaagerup.passesTriplePointObstruction_? should equal(false)
  }
  
  "isomorphicTo_?" should "work" in {
    val g0 = PairOfBigraphsWithDuals("bwd1v1v1v1p1p1v1x0x0p1x0x0p0x1x0v0x0x1duals1v1v1x2x3v1", "bwd1v1v1v1p1p1v1x0x0p0x1x0p0x0x1v0x0x1p1x0x0p0x1x0duals1v1v1x2x3v1x3x2")
    val g1 = PairOfBigraphsWithDuals("bwd1v1v1v1p1p1v1x0x0p1x0x0p0x1x0v0x0x1duals1v1v1x2x3v1", "bwd1v1v1v1p1p1v1x0x0p0x1x0p0x0x1v0x0x1p0x1x0p1x0x0duals1v1v1x2x3v1x3x2")
    g0.canonicalNautyGraph should equal(g1.canonicalNautyGraph)
  }

}