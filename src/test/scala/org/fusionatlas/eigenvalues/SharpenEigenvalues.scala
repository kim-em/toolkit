package org.fusionatlas.eigenvalues

import net.tqft.toolkit.algebra.Matrix
import net.tqft.toolkit.algebra.Matrices
import net.tqft.toolkit.algebra.OrderedField
import net.tqft.toolkit.algebra.Gadgets.FractionalField
import net.tqft.toolkit.algebra.Implicits
import net.tqft.toolkit.functions.FixedPoint

object SharpenEigenvalues extends App {
  import Implicits.BigDecimals

  def sharpenEigenvalueOneStep(matrix: Matrix[BigDecimal], estimate: BigDecimal, bounds: (BigDecimal, BigDecimal), approximateEigenvector: List[BigDecimal]): (BigDecimal, List[BigDecimal]) = {
    require(matrix.numberOfColumns == matrix.numberOfRows)
    val matrices = Matrices.matricesOver(BigDecimals, matrix.numberOfRows)

    def sqrt(x: BigDecimal): BigDecimal = {
      val initialGuess = new BigDecimal(x.underlying.scaleByPowerOfTen(-x.scale / 2), x.mc)
      val result = FixedPoint({ g: BigDecimal => ((x / g) + g) / 2 })(initialGuess).abs
      result
    }
    def norm(vector: List[BigDecimal]) = sqrt(vector.map(d => d.pow(2)).reduceLeft(_ + _))
    def normalize(vector: List[BigDecimal]) = {
      val n = norm(vector) * vector.head.signum
      vector map { x => x / n }
    }

    val checkedEstimate = if (estimate < bounds._1) {
      bounds._1
    } else if (estimate > bounds._2) {
      bounds._2
    } else {
      estimate
    }

    val mm = matrices.subtract(matrix, matrices.scalarMultiply(checkedEstimate, matrices.identityMorphism()))
    //    println(mm)
    //    println(mm.rowEchelonForm)
    //    println(mm.reducedRowEchelonForm)
    val aa = mm.preimageOf(approximateEigenvector).getOrElse(approximateEigenvector)
    //    println(aa)
    val improvedEigenvector = normalize(aa)
    val newEstimate = norm(matrix(improvedEigenvector))
    (newEstimate, improvedEigenvector)
  }

  def sharpenEigenvalue(matrix: Matrix[BigDecimal], initialEstimate: BigDecimal, bounds: (BigDecimal, BigDecimal), approximateEigenvector: List[BigDecimal]): (BigDecimal, List[BigDecimal]) = {
    def f(p: (BigDecimal, List[BigDecimal])) = {
      val r = sharpenEigenvalueOneStep(matrix, p._1, bounds, p._2)
      println(r)
      r
    }
    import FixedPoint._
    (f _).firstRepeatedIteration(((initialEstimate, approximateEigenvector)))
  }

  
  def sharpenEigenvalue(matrix: Matrix[Int], bounds: (BigDecimal, BigDecimal), precision: Int): (BigDecimal, List[BigDecimal]) = {
    val (mc, initialEstimate, initialEigenvector) =
      if (precision < 32) {
        val mc = java.math.MathContext.DECIMAL32
        (mc, ((bounds._1 + bounds._2) / 2)(mc), List.fill(matrix.numberOfColumns)(BigDecimal(scala.util.Random.nextDouble(), mc)))
      } else {
        val mc = new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN)
        val (previousEstimate, previousEigenvector) = sharpenEigenvalue(matrix, bounds, precision / 2)
        (mc, previousEstimate(mc), previousEigenvector map { x => x(mc) })
      }
    val sharpBounds = (bounds._1(mc), bounds._2(mc))
    val decimalMatrix = Matrix(matrix.entries.map { row => row.map { x => BigDecimal(x, mc)}})
    sharpenEigenvalue(decimalMatrix, initialEstimate, sharpBounds, initialEigenvector)
  }

  val matrix = Matrix(List(List(7,1), List(2,5)))
  val decimalMatrix = Matrix(List(List(BigDecimal(7), BigDecimal(1)), List(BigDecimal(2), BigDecimal(5))))
  val initialEstimate = BigDecimal(5)
  val bounds = (BigDecimal(4.1), BigDecimal(4.3))
  val approximateEigenvector = List(BigDecimal(0.1), BigDecimal(0.2))

//  println(sharpenEigenvalue(decimalMatrix, initialEstimate, bounds, approximateEigenvector))
  
  println(sharpenEigenvalue(matrix, bounds, 48))
}