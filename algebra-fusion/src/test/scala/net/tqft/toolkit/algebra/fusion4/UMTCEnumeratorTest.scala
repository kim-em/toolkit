package net.tqft.toolkit.algebra.fusion4

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

@RunWith(classOf[JUnitRunner])
class UMTCEnumeratorTest extends FlatSpec with Matchers {

  "firstNonInvertibleObjectMatrices" should "find all the initial matrices" in {
    val rank =6
    val enumerator = UMTCEnumerator(SmallGroup(1, 0), 80.0)
    val orbits = enumerator.OrbitStructure(Seq.fill(rank - 1)(SelfDualOrbit(GxGOrbit(0, 0), (0, 0))), Seq.empty)
    val matrices = orbits.firstNonInvertibleObjectMatrices
    //    println(matrices.size)
//    for (m <- orbits.firstNonInvertibleObjectMatricesWithEigendata; d <- m.diagonalisation; s <- d.symmetrised; N <- s.verlindeMultiplicities) {
//      val max = N.map(_.map(_.max).max).max
//      val sep = if (max >= 10) "," else ""
//      println(rank + ",0 " + max + " " + N.map(_.map(_.mkString(sep)).mkString(sep)).mkString(sep))
//      //      println(N.map(_.map(_.mkString("{",",","}")).mkString("{",",","}")).mkString("{",",","}"))
//      //      println(N.transpose.map(_.map(_.mkString).mkString(" ")).mkString("\n"))
//      //      println
//    }

    import net.tqft.toolkit.collections.Tally._
    import Ordering.Implicits._
    val eigenspaceSizes = (for (m <- orbits.firstNonInvertibleObjectMatricesWithEigendata; if m.diagonalisation.isEmpty) yield {
      m.eigenspaces.map(_.eigenbasis.size).sorted
    }).tally.toSeq.sorted

    for (p <- eigenspaceSizes) println(p)

  }

}