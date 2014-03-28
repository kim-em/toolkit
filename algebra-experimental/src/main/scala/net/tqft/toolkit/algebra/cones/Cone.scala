package net.tqft.toolkit.algebra.cones

import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.VectorSpace
import net.tqft.toolkit.algebra.OrderedField
import net.tqft.toolkit.algebra.fusion.FiniteDimensionalVectorSpace
import net.tqft.toolkit.algebra.Integers
import net.tqft.toolkit.permutations.Permutations
import net.tqft.toolkit.algebra.ApproximateReals

trait Cone[F, V] {
  def enumeratePointsBelowWall(wall: V => Double, limit: Double)(implicit vectorSpace: VectorSpace[F, V]): Iterator[V]
}

trait PolyhedralCone[F, V] extends Cone[F, V] {
  def decomposeIntoSimplicialCones: Iterator[SimplicialCone[F, V]]
  override def enumeratePointsBelowWall(wall: V => Double, limit: Double)(implicit vectorSpace: VectorSpace[F, V]): Iterator[V] = {
    for (simplicialCone <- decomposeIntoSimplicialCones; p <- simplicialCone.enumeratePointsBelowWall(wall, limit)) yield p
  }
}

case class CubicalCone[F, V](rank: Int, generators: Map[Seq[Int], V]) extends PolyhedralCone[F, V] {
  override def decomposeIntoSimplicialCones = {
    val keys = {
      import net.tqft.toolkit.collections.CartesianProduct._
      List.fill(rank)(List(0, 1)).cartesianProduct
    }
    import net.tqft.toolkit.permutations.Permutations._
    for (p <- Permutations.of(rank)) yield {
      SimplicialCone[F, V]((for (k <- keys; pk = p.permute(k); if pk == pk.sorted) yield {
    	  generators(k)
      }).toSeq)
    }
  }
}

case class SimplicialCone[F, V](generators: Seq[V]) extends PolyhedralCone[F, V] {
  override def decomposeIntoSimplicialCones = Iterator(this)
  override def enumeratePointsBelowWall(wall: V => Double, limit: Double)(implicit vectorSpace: VectorSpace[F, V]): Iterator[V] = {
    val generatorSizes = generators.map(wall)
    for (s <- generatorSizes) {
      require(ApproximateReals.Doubles.positive_?(s))
    }
    def nextIterator(p: Iterator[(Seq[Int], Double)], s: Double): Iterator[(Seq[Int], Double)] = for((c, l) <- p; d <- 0 until scala.math.floor(l/s).toInt) yield (c :+ d, l - d * s)
    val iterator: Iterator[Seq[Int]] = (generatorSizes).foldLeft(Iterator((Seq[Int](), limit)))(nextIterator _).map(_._1)
    iterator.map(c => vectorSpace.sum(c.zip(generators).map({ p => vectorSpace.scalarMultiply(vectorSpace.coefficients.fromInt(p._1), p._2) })))
  }
}