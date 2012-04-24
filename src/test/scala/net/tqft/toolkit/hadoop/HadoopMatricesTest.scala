package net.tqft.toolkit.hadoop

import net.tqft.toolkit.algebra.Implicits
import net.tqft.toolkit.algebra.Matrix
import net.tqft.toolkit.algebra.Fraction

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HadoopMatricesTest extends FlatSpec with ShouldMatchers {
  import Implicits.Integers
  import Implicits.Rationals
  import Implicits.integersAsRationals
  import Implicits.Doubles

    "hadoopiness" should "make me happy" in {
      import net.tqft.toolkit.hadoop.HadoopSeq
      val m: Matrix[Fraction[Int]] = Matrix(3, HadoopSeq(List(20, 12, 1), List(12, 60, 1)))
      //    val r: Matrix[Fraction[Int]] = Matrix(3, List(List(1,Fraction(3,5),Fraction(1,20)), List(0,1,Fraction(1,132))))
      val r: Matrix[Fraction[Int]] = Matrix(3, List(List(20, 12, 1), List(0, Fraction(264, 5), Fraction(2, 5))))
  
      
      m.rowEchelonForm should equal(r)
    }

  "hadoopiness" should "work with large matrices" in {
    import org.fusionatlas.eigenvalues._
    val target = Hadamard12.transfer2
    val hadoopTarget = new Matrix(target.numberOfColumns, HadoopSeq.from(target.entries))
    val bounds = (19.9, 20.2)
    SharpenEigenvalues.findEigenvalue(hadoopTarget, bounds, 100)
  }
}
