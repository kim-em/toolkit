package net.tqft.toolkit.algebra.fusion4

import org.jblas.Eigen
import org.jblas.DoubleMatrix
import org.jblas.ComplexDouble
import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues
import java.io.FileOutputStream
import java.io.PrintWriter

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
        case Some((i, j)) => mergeEigenspaces(eigenspaces.updated(i, Eigenspace(eigenspaces(i).eigenvalue, eigenspaces(i).eigenbasis ++ eigenspaces(j).eigenbasis)).take(j) ++ eigenspaces.drop(j + 1))
      }
    }
    mergeEigenspaces(preliminaryEigenspaces)
  }

  private def numericallyDistinctIndices(z: Seq[Double]): Seq[Int] = {
    for ((w, i) <- z.zipWithIndex; if z.count(y => (y - w).abs < 0.001) == 1) yield i
  }

  val rank = 7
  val globalDimension = 80.0

  val startTime = System.currentTimeMillis

  val matrices = FirstMatrices(rank, scala.math.sqrt((globalDimension - 1) / (rank - 1))).toVector
  
  println(s"There are a total of ${matrices.size} matrices. (${(System.currentTimeMillis - startTime) / 1000}s)")

  val out = new PrintWriter(new FileOutputStream("out"))
  for(m <- matrices) out.println(m.map(_.mkString).mkString)

  println(s"Written matrices to disk. (${(System.currentTimeMillis - startTime) / 1000}s)")

//  val smallestMatrices = for (
//    m <- matrices;
//    v0 = FrobeniusPerronEigenvalues.estimateWithEigenvector(m, 0.0000001)._2;
//    v = v0.map(x => x / v0(0));
//    if v.tail.tail.forall(x => x > v(1) - 0.01)
//  ) yield m
//

  val matricesWithEigensystems = matrices.map(m => (m, symmetricEigensystem(m)))

  val smallestObjectMatrices = matricesWithEigensystems.filter({ case (m, (eigenvalues, eigenvectors)) =>
    val i = eigenvalues.indexOf(eigenvalues.max)
    val v = eigenvectors(i)
    v.tail.tail.forall(x => x > 0.99 * v(1))
  })

  println(s"Of those, ${smallestObjectMatrices.size} are the smallest object. (${(System.currentTimeMillis - startTime) / 1000}s)")
  
  val (todo, done) = smallestObjectMatrices.partition(p => numericallyDistinctIndices(p._2._1).size < rank)

  println(s"Of these ${done.size} already have distinct eigenvalues.  (${(System.currentTimeMillis - startTime) / 1000}s)")

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