package org.fusionatlas.eigenvalues

import net.tqft.toolkit.algebra.Matrix
import net.tqft.toolkit.algebra.Matrices
import net.tqft.toolkit.algebra.OrderedField
import net.tqft.toolkit.algebra.Gadgets.FractionalField
import net.tqft.toolkit.algebra.Implicits
import net.tqft.toolkit.functions.FixedPoint

object SharpenEigenvalues extends App {
  import Implicits.BigDecimals

  def sharpenEigenvalue(matrix: Matrix[BigDecimal], estimate: BigDecimal, bounds: (BigDecimal, BigDecimal), approximateEigenvector: List[BigDecimal]): (BigDecimal, List[BigDecimal]) = {
    require(matrix.numberOfColumns == matrix.numberOfRows)
    val matrices = Matrices.matricesOver(BigDecimals, matrix.numberOfRows)

    def sqrt(x: BigDecimal): BigDecimal = {
      val initialGuess = new BigDecimal(x.underlying.scaleByPowerOfTen(-x.scale / 2), x.mc)
      FixedPoint({ g: BigDecimal => ((x / g) + g) / 2 })(initialGuess)
    }
    def norm(vector: List[BigDecimal]) = sqrt(vector.map(d => d.pow(2)).foldLeft(BigDecimals.zero)(BigDecimals.add _))
    def normalize(vector: List[BigDecimal]) = {
      val n = norm(vector)
      vector map { x => x / n }
    }

    val mm = matrices.subtract(matrix, matrices.scalarMultiply(estimate, matrices.identityMorphism()))
    println(mm)
    println(mm.rowEchelonForm)
    println(mm.reducedRowEchelonForm)
    val aa = mm.preimageOf(approximateEigenvector).get
    println(aa)
    val improvedEigenvector = normalize(aa)
    val newEstimate = norm(matrix(improvedEigenvector))
    val checkedEstimate = if (newEstimate < bounds._1 || newEstimate > bounds._2) { estimate } else { newEstimate }
    (checkedEstimate, improvedEigenvector)
  }

  val matrix = Matrix(List(List(BigDecimal(5), BigDecimal(1)), List(BigDecimal(2), BigDecimal(5))))
  val initialEstimate = BigDecimal(3.6)
  val bounds = (BigDecimal(3.5), BigDecimal(3.7))
  val approximateEigenvector = List(BigDecimal(0.1), BigDecimal(0.2))
  
  def f(p: (BigDecimal, List[BigDecimal])) = {
    val r = sharpenEigenvalue(matrix, p._1, bounds, p._2)
    println(r)
    r
  }
  
  FixedPoint(f)(((initialEstimate, approximateEigenvector)))
}