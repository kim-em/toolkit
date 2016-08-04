package net.tqft.toolkit.algebra.fusion4

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

@RunWith(classOf[JUnitRunner])
class UMTCEnumeratorTest extends FlatSpec with Matchers {

  "firstNonInvertibleObjectMatrices" should "find all the initial matrices" in {
    val rank = 7
    val globalDimensionBound = 60.0
    val enumerator = UMTCEnumerator(rank, 0, globalDimensionBound)
//    val matrices = enumerator.firstNonInvertibleObjectMatrices
    //    println(matrices.size)
    //    for (m <- enumerator.firstNonInvertibleObjectMatricesWithEigendata; d <- m.diagonalisation; s <- d.symmetrised; N <- s.verlindeMultiplicities) {
    //    }
    for (m <- enumerator.firstNonInvertibleObjectMatricesWithEigendata) {
      if (m.diagonalisationOrPartialFusionRing.isRight) {
//        println("Eigenspaces: " + m.eigenspaces.map(_.eigenbasis.size).sorted)

        val fusion3Enumerator = net.tqft.toolkit.algebra.fusion3.Enumeration(rank, 0,
          globalDimensionBound,
          true,
          Some(m.eigenspaces.map(_.eigenvalue.abs).max - 0.01),
          None,
          Some(m.m))
        try {
          for (c <- fusion3Enumerator.root.descendants) {
            println(c.toString)
          }
        } catch {
          case e: NoSuchElementException => {

          }
        }
      } else {
        val d = m.diagonalisationOrPartialFusionRing.left.get
        d.symmetrised.map({ s =>
          s.verlindeMultiplicities.map({ N =>
//            println(m.m.map(_.mkString).mkString("\n"))
//            println
//            println(N(1).map(_.mkString).mkString("\n"))
        val max = N.map(_.map(_.max).max).max
            val sep = if (max >= 10) "," else ""
            println(rank + ",0 " + max + " " + N.map(_.map(_.mkString(sep)).mkString(sep)).mkString(sep))
            //      println(N.map(_.map(_.mkString("{",",","}")).mkString("{",",","}")).mkString("{",",","}"))
            //      println(N.transpose.map(_.map(_.mkString).mkString(" ")).mkString("\n"))
            //      println
          })
        })

      }
    }

    //    import net.tqft.toolkit.collections.Tally._
    //    import Ordering.Implicits._
    //    val eigenspaceSizes = (for (m <- enumerator.firstNonInvertibleObjectMatricesWithEigendata; pfr <- m.diagonalisationOrPartialFusionRing.right.toOption) yield {
    //      m.eigenspaces.map(_.eigenbasis.size).sorted
    //    }).tally.toSeq.sorted
    //
    //    for (p <- eigenspaceSizes) println(p)

  }

}