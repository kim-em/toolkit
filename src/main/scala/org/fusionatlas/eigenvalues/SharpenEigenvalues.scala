package org.fusionatlas.eigenvalues

import net.tqft.toolkit.algebra.Matrix
import net.tqft.toolkit.algebra.Matrices
import net.tqft.toolkit.algebra.OrderedField
import net.tqft.toolkit.algebra.Gadgets.FractionalField
import net.tqft.toolkit.algebra.Implicits
import net.tqft.toolkit.functions.FixedPoint
import net.tqft.toolkit.algebra.Gadgets

  import java.math.{ BigDecimal => jBigDecimal } 


object SharpenEigenvalues extends App {
  case class EigenvalueEstimate(approximateEigenvalue: BigDecimal, approximateEigenvector: List[BigDecimal])

  def sharpenEigenvalueOneStep(matrix: Matrix[Byte], bounds: (BigDecimal, BigDecimal))(estimate: EigenvalueEstimate): EigenvalueEstimate = {
    require(matrix.numberOfColumns == matrix.numberOfRows)
    val mc = estimate.approximateEigenvalue.mc
    implicit val bigDecimals = Gadgets.BigDecimals(mc)

    val matrices = Matrices.matricesOver(bigDecimals, matrix.numberOfRows)

    def sqrt(x: BigDecimal): BigDecimal = {
      val initialGuess = new BigDecimal(x.underlying.scaleByPowerOfTen(-x.scale / 2), x.mc)
      val result = FixedPoint({ g: BigDecimal => ((x / g) + g) / 2 })(initialGuess).abs
      result
    }
    def norm(vector: List[BigDecimal]) = sqrt(vector.map(d => d.pow(2)).reduceLeft(_ + _))
    def normalize(vector: List[BigDecimal]) = {
      val n = norm(vector) //* vector.head.signum
      val result = vector map { x => x / n }
      require(result.head.mc.getPrecision == vector.head.mc.getPrecision)
      result
    }

    val checkedEstimate = bounds._1.min(bounds._2.max(estimate.approximateEigenvalue))

    val mm = Matrix(matrix.entries.zipWithIndex.map {
      case (row: scala.collection.mutable.WrappedArray[_], index) => {
        val decimalRow = row.map(x => BigDecimal(x.toInt, mc))
        decimalRow(index) = decimalRow(index) - checkedEstimate
        decimalRow
      } 
    })
    val aa = mm.preimageOf(estimate.approximateEigenvector).getOrElse(estimate.approximateEigenvector)
    val improvedEigenvector = normalize(aa)
    val action = for(row <- matrix.entries) yield {
      (improvedEigenvector zip row).map(p => p._1 * p._2).sum
    }
    val newEstimate = norm(action)
    println(newEstimate)
    EigenvalueEstimate(newEstimate, improvedEigenvector)
  }

  def sharpenEigenvalue(matrix: Matrix[Byte], bounds: (BigDecimal, BigDecimal), precision: Int)(estimate: EigenvalueEstimate): EigenvalueEstimate = {
    import FixedPoint._
    val field = Gadgets.BigDecimals(precision)
    val fixedPoint = FixedPoint.sameTest({ (p: EigenvalueEstimate, q: EigenvalueEstimate) => field.chop(p.approximateEigenvalue - q.approximateEigenvalue, field.epsilon * 1000) == field.zero }) _
    fixedPoint(sharpenEigenvalueOneStep(matrix, bounds) _)(estimate)
  }

  def findEigenvalueAndEigenvector(matrix: Matrix[Byte], bounds: (Double, Double), precision: Int): EigenvalueEstimate = {
    val (mc, initialEstimate) =
      if (precision < 34) {
        val mc = java.math.MathContext.DECIMAL128
        //        val mc = new java.math.MathContext(100, java.math.RoundingMode.HALF_EVEN)
        (mc, EigenvalueEstimate(BigDecimal(bounds._1 + (bounds._2 - bounds._1) * scala.util.Random.nextDouble, mc), List.fill(matrix.numberOfColumns)(BigDecimal(scala.util.Random.nextDouble(), mc))))
      } else {
        val mc = new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN)
        val EigenvalueEstimate(previousEstimate, previousEigenvector) = findEigenvalueAndEigenvector(matrix, bounds, 1 * precision / 2)
        (mc, EigenvalueEstimate(previousEstimate(mc), previousEigenvector map { x => x(mc) }))
      }
    val sharpBounds = (BigDecimal(bounds._1, mc), BigDecimal(bounds._2, mc))
    sharpenEigenvalue(matrix, sharpBounds, precision)(initialEstimate)
  }

  def findEigenvalue(matrix: Matrix[Byte], bounds: (Double, Double), precision: Int) = findEigenvalueAndEigenvector(matrix, bounds, precision).approximateEigenvalue

  val matrix = Matrix(List(List(7, 1), List(2, 5)).map(_.map(_.toByte)))
  val bounds = (4.1, 4.3)

  while (true) {
//    println(findEigenvalue(Hadamard12.transfer2, (19.9, 20.2), 100))
    println(findEigenvalue(Hadamard12.transfer3, (59.9, 60.1), 100))
  }
  //  println(findEigenvalue(matrix, bounds, 100))
}
