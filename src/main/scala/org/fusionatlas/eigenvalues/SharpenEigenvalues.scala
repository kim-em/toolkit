package org.fusionatlas.eigenvalues

import net.tqft.toolkit.algebra.Matrix
import net.tqft.toolkit.algebra.Matrices
import net.tqft.toolkit.algebra.OrderedField
import net.tqft.toolkit.algebra.ApproximateField
import net.tqft.toolkit.algebra.ApproximateReals
import net.tqft.toolkit.algebra.Gadgets.FractionalField
import net.tqft.toolkit.algebra.Implicits
import net.tqft.toolkit.functions.FixedPoint
import net.tqft.toolkit.algebra.Gadgets
import org.apfloat.Apfloat

import net.tqft.toolkit.hadoop.WiredCollections._
import com.nicta.scoobi.Scoobi.WireFormat

import java.math.{ BigDecimal => jBigDecimal }

object SharpenEigenvalues extends App {
  case class EigenvalueEstimate[D](approximateEigenvalue: D, approximateEigenvector: List[D])

  def sharpenEigenvalueOneStep[D: Manifest: WireFormat: ApproximateField](matrix: Matrix[Byte], bounds: (D, D)): EigenvalueEstimate[D] => EigenvalueEstimate[D] = { estimate =>
    println("sharpening one step")

    val field = implicitly[ApproximateField[D]]
    import field._

    val matrices = Matrices.matricesOver(field, matrix.numberOfRows)
    val checkedEstimate = min(bounds._2, max(bounds._1, estimate.approximateEigenvalue))

    val mm = Matrix(matrix.entries.zipWithIndex.wireMap {
      case (row: scala.collection.mutable.WrappedArray[_], index) => {
        val decimalRow = row.map({ x: Byte => field.fromInt(x.toInt) })
        decimalRow(index) = subtract(decimalRow(index), checkedEstimate)
        decimalRow
      }
    })
    val aa = mm.preimageOf(estimate.approximateEigenvector).getOrElse(estimate.approximateEigenvector)
    val improvedEigenvector = normalize(aa)
    val action = (for (row <- matrix.entries) yield {
      (improvedEigenvector zip row).map(p => multiply(p._1, p._2)).reduceLeft(add(_, _))
    }).toList
    val newEstimate = norm(action)
    println("new eigenvalue estimate: " + newEstimate)
    println("new approximate eigenvector: " + improvedEigenvector)
    EigenvalueEstimate(newEstimate, improvedEigenvector)
  }

  def sharpenEigenvalue[D: Manifest: WireFormat: ApproximateField](matrix: Matrix[Byte], bounds: (D, D)): EigenvalueEstimate[D] => EigenvalueEstimate[D] = { estimate =>
    val field = implicitly[ApproximateField[D]]
    import FixedPoint._
    val fixedPoint = FixedPoint.sameTest({ (p: EigenvalueEstimate[D], q: EigenvalueEstimate[D]) => field.chop(field.subtract(field.quotient(p.approximateEigenvalue, q.approximateEigenvalue), field.one), field.multiply(field.epsilon, 1000)) == field.zero }) _
    fixedPoint(sharpenEigenvalueOneStep(matrix, bounds) _)(estimate)
  }

  def findEigenvalueAndEigenvector(matrix: Matrix[Byte], bounds: (Double, Double), precision: Int): EigenvalueEstimate[Apfloat] = {
    require(matrix.numberOfColumns == matrix.numberOfRows)

    implicit val field: ApproximateReals[Apfloat] = Gadgets.Apfloats(precision)
    val initialEstimate =
      if (precision < 34) {
        EigenvalueEstimate(field.fromDouble(bounds._1 + (bounds._2 - bounds._1) * 0.61239873 /* scala.util.Random.nextDouble */ ), List.fill(matrix.numberOfColumns)(field.fromDouble(scala.util.Random.nextDouble())))
      } else {
        println("halving precision...")
        val EigenvalueEstimate(previousEstimate, previousEigenvector) = findEigenvalueAndEigenvector(matrix, bounds, 1 * precision / 2)
        println("increasing precision of the estimates...")
        EigenvalueEstimate(field.setPrecision(previousEstimate), previousEigenvector map { x => field.setPrecision(x) })
      }
    val sharpBounds = (field.fromDouble(bounds._1), field.fromDouble(bounds._2))
    import com.nicta.scoobi.WireFormat._
    implicit val ApfloatManifest = implicitly[Manifest[Apfloat]]
    implicit val ApfloatWireFormat = new WireFormat[Apfloat] {
      override def fromWire(in: java.io.DataInput): Apfloat = {
        val l = in.readInt()
        val b = new Array[Byte](l)
        in.readFully(b, 0, l)
        new Apfloat(new String(b, "utf-8"))
      }
      override def toWire(x: Apfloat, out: java.io.DataOutput) {
        val b = x.toString(false).getBytes("utf-8")
        out.writeInt(b.length)
        out.write(b)
      }
    }
    sharpenEigenvalue(matrix, sharpBounds)(ApfloatManifest, ApfloatWireFormat, field)(initialEstimate)
  }

  def findEigenvalue(matrix: Matrix[Byte], bounds: (Double, Double), precision: Int) = findEigenvalueAndEigenvector(matrix, bounds, precision).approximateEigenvalue

  println("default parallelism was: " + collection.parallel.ForkJoinTasks.defaultForkJoinPool.getParallelism)
  collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(collection.parallel.ForkJoinTasks.defaultForkJoinPool.getParallelism * 2)

  val (target, bounds) = args(0) match {
    case "2" => (Hadamard12.transfer2, (19.9, 20.2))
    case "3" => (Hadamard12.transfer3, (59.9, 60.1))
    case "4" => (Hadamard12.transfer4, (353.95, 352.97))
  }

  println(findEigenvalue(target, bounds, 100))
}
