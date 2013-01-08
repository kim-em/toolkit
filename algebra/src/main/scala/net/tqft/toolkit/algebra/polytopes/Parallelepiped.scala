package net.tqft.toolkit.algebra.polytopes

import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.Conversions
import net.tqft.toolkit.algebra.AdditiveGroup
import net.tqft.toolkit.algebra.VectorOperations
import net.tqft.toolkit.Logging
import net.tqft.toolkit.algebra.matrices.Matrices
import net.tqft.toolkit.algebra.Rationals
import net.tqft.toolkit.algebra.ApproximateReals

object Parallelepiped extends Logging

case class Parallelepiped(generators: Seq[Seq[Int]]) {
  require(generators.nonEmpty)
  require(generators.size == generators.head.size)

  private lazy val exactInverse = (generators: Matrix[Int]).transpose.mapEntries(Conversions.integersAsBigRationals).inverse.get
  private lazy val approximateInverse = (generators: Matrix[Int]).transpose.mapEntries(Conversions.integersAsDoubles).inverse.get

  def contains(point: Seq[Int]): Boolean = {
    val approximateCoordinates = approximateInverse.apply(point.map(Conversions.integersAsDoubles))
    if (approximateCoordinates.forall(x => x >= 0.0001 && x <= 0.9999)) {
      true
    } else if (approximateCoordinates.exists(x => x < -0.0001 || x > 1.0001)) {
      false
    } else {
      exactInverse.apply(point.map(Conversions.integersAsBigRationals)).forall(x => x.numerator >= 0 && x.numerator < x.denominator)
    }

  }

  lazy val volume = (generators: Matrix[Int]).mapEntries(Conversions.integersAsRationals).determinant.numerator.abs

  def enumeratePointsUsingMesh(scaleFactors: Seq[Double]): Set[Seq[Int]] = {
      import net.tqft.toolkit.collections.CartesianProduct._

      val scaledGenerators = for ((s, g) <- scaleFactors.zip(generators)) yield (for (x <- g) yield s * x)

    val interiorIterator = {
      val iterators = scaleFactors.map(s => (1 to scala.math.floor(1.0 / s).toInt))
      iterators.cartesianProduct
    }
    
    val boundaryIterator = {
      val iterators = scaleFactors.map(s => (0 to scala.math.ceil(1.0 / s).toInt))
      val faceIterators = for(i <- (0 until generators.size).toIterator) yield {
        (iterators.updated(i, Seq(iterators(i).head, iterators(i).last))).cartesianProduct
      }
      faceIterators.flatten
    }

    def roundings(point: Seq[Double]): Iterator[Seq[Int]] = {
      (for (p <- point) yield Seq(p.floor.toInt, p.ceil.toInt)).cartesianProduct
    }
    def meshPoint(coordinates: Seq[Int]) = (for ((c, g) <- coordinates.zip(scaledGenerators)) yield (for (x <- g) yield c * x)).reduce(VectorOperations.add[Double])

    val interiorPoints = interiorIterator.grouped(20000).flatMap(points => points.par.map(p => meshPoint(p).map(x => x.floor.toInt)).toSet.filter(contains)).toSet
    val boundaryPoints = boundaryIterator.grouped(20000).flatMap(points => points.par.flatMap(p => roundings(meshPoint(p))).toSet.filter(contains)).toSet
    
    (interiorPoints ++ boundaryPoints).ensuring(_.size == volume)
  }
}