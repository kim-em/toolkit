package net.tqft.toolkit.hadoop

import net.tqft.toolkit.algebra.Implicits
import net.tqft.toolkit.algebra.Matrix
import net.tqft.toolkit.algebra.Fraction

import com.nicta.scoobi.Scoobi.WireFormat


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
      import net.tqft.toolkit.hadoop.HadoopMatrix
      implicit def fractionWireFormat[A:WireFormat]: WireFormat[Fraction[A]] = new WireFormat[Fraction[A]] {
        val aWireFormat = implicitly[WireFormat[A]]
        def fromWire(in: java.io.DataInput) = {
          Fraction.alreadyReduced(aWireFormat.fromWire(in), aWireFormat.fromWire(in))
        }
        def toWire(x: Fraction[A], out: java.io.DataOutput) {
          aWireFormat.toWire(x.numerator, out)
          aWireFormat.toWire(x.denominator, out)
        }
      }
      val m: Matrix[Fraction[Int]] = HadoopMatrix[Fraction[Int]](3, List(List(20, 12, 1), List(12, 60, 1)))
      //    val r: Matrix[Fraction[Int]] = Matrix(3, List(List(1,Fraction(3,5),Fraction(1,20)), List(0,1,Fraction(1,132))))
      val r: Matrix[Fraction[Int]] = Matrix(3, List(List(20, 12, 1), List(0, Fraction(264, 5), Fraction(2, 5))))
  
      
      val result = m.rowEchelonForm
      r should equal(result)
    }

}
