package net.tqft.toolkit.algebra.fusion4

import org.jblas.Eigen
import org.jblas.DoubleMatrix
import org.jblas.ComplexDouble

object FirstMatricesApp extends App {

  private def symmetricEigensystem(m: Array[Array[Int]]) = {
    val Seq(eigenvectors, diagonal) = Eigen.symmetricEigenvectors(new DoubleMatrix(m.map(_.map(_.toDouble)))).toSeq
    val eigenvalues = {
      val d = diagonal.toArray2
      for (i <- 0 until m.length) yield d(i)(i)
    }
    (eigenvalues, eigenvectors.toArray2.transpose.toIndexedSeq)
  }

  case class Eigenspace(eigenvalue: Double, eigenbasis: Seq[Array[Double]]) {
    lazy val projection = ???
  }

  private def eigenspaces(m: Array[Array[Int]]): Seq[Eigenspace] = {
    val preliminaryEigenspaces = (for ((e, x) <- symmetricEigensystem(m).zipped) yield Eigenspace(e, Seq(x))).toSeq
    def mergeEigenspaces(eigenspaces: Seq[Eigenspace]): Seq[Eigenspace] = {
      (for (((e, i), tails) <- eigenspaces.iterator.zipWithIndex.zip(eigenspaces.tails.toSeq.tail.iterator); (f, j) <- tails.zipWithIndex; if (e.eigenvalue - f.eigenvalue).abs < 0.001) yield {
        (i, i + j + 1)
      }).toStream.headOption match {
        case None => eigenspaces
        case Some((i, j)) => mergeEigenspaces(eigenspaces.updated(i, Eigenspace(eigenspaces(i).eigenvalue, eigenspaces(i).eigenbasis ++ eigenspaces(j).eigenbasis)).take(j ) ++ eigenspaces.drop(j + 1))
      }
    }
    mergeEigenspaces(preliminaryEigenspaces)
  } 

  private def numericallyDistinctIndices(z: Seq[Double]): Seq[Int] = {
    for ((w, i) <- z.zipWithIndex; if z.count(y => (y-w).abs < 0.001) == 1) yield i
  }

  val rank = 6
  val globalDimension = 60.0

  val matrices = (for (m <- FirstMatrices(rank, scala.math.sqrt((globalDimension - 1) / (rank - 1))); e = symmetricEigensystem(m)._1) yield (m, e)).toStream

  val (todo, done) = matrices.partition(p => numericallyDistinctIndices(p._2).size < rank)

  println(s"Of a total of ${matrices.size} matrices, ${done.size} already have distinct eigenvalues.")

  val sizes = {
    import net.tqft.toolkit.collections.Tally._
    todo.map(p => eigenspaces(p._1).map(_.eigenbasis.size).sorted).tally
  }
  import Ordering.Implicits._
  println(sizes.toSeq.sorted.mkString("\n"))
  
  println(eigenspaces(todo.head._1).map(_.eigenbasis.map(_.mkString("{", ", ", "}")).mkString("{", ", ", "}")).mkString("\n"))
  
  //  for((m,e) <- todo) {
  //    println
  //    println(m.map(_.mkString(" ")).mkString("\n"))
  //    println(e)
  //  }
}