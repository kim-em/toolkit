package net.tqft.toolkit.algebra.fusion4

import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues
import org.jblas.DoubleMatrix
import org.jblas.Eigen
import org.jblas.ComplexDouble
import net.tqft.toolkit.arithmetic.Sum

case class SmallGroup(order: Int, index: Int)
case class GxGOrbit(name: Int, index: Int)
case class SelfDualOrbit(orbit: GxGOrbit, dual: (Int, Int)) // X^* = dual._1 X dual._2
case class DualPairOrbit(orbit1: GxGOrbit, orbit2: GxGOrbit) // X^* = Y

case class UMTCEnumerator(invertibles: SmallGroup, globalDimension: Double) {
  require(invertibles.order == 1)

  case class OrbitStructure(selfDualOrbits: Seq[SelfDualOrbit], dualPairOrbits: Seq[DualPairOrbit]) {
    require(selfDualOrbits.forall(_.orbit.name == 0))
    require(dualPairOrbits.isEmpty)

    def firstNonInvertibleObjectMatrices: Iterator[Array[Array[Int]]] = {
      val rank = selfDualOrbits.size + 1
      val initialMatrix = Array.fill(rank, rank)(0)
      initialMatrix(0)(1) = 1
      initialMatrix(1)(0) = 1
      val eigenvalueBound = scala.math.sqrt((globalDimension - 1) / (rank - 1))
      val coefficientClusters = (for (i <- 1 until rank; j <- 1 to i) yield Set((i, j), (j, i)).toList).toArray

      SymmetricMatrixEnumerator(initialMatrix, eigenvalueBound, coefficientClusters)
    }

    case class Eigenspace(eigenvalue: ComplexDouble, eigenbasis: Seq[Array[ComplexDouble]]) {
      lazy val projection = ???
      def normalise: Eigenspace = {
        Eigenspace(eigenvalue, eigenbasis.map({ v =>
          val norm = scala.math.sqrt(v.map(x => scala.math.pow(x.abs, 2)).sum)
          val f = v(0).div(v(0).abs).mul(norm)
          v.map(x => x.div(f))
        }))
      }
    }

    private val eigenvalueTolerance = 0.001

    private def symmetricEigensystem(m: Array[Array[Int]]) = {
      val Seq(eigenvectors, diagonal) = Eigen.symmetricEigenvectors(new DoubleMatrix(m.map(_.map(_.toDouble)))).toSeq
      val eigenvalues = {
        val d = diagonal.toArray2
        for (i <- 0 until m.length) yield d(i)(i)
      }
      (eigenvalues, eigenvectors.toArray2.transpose.toIndexedSeq)
    }
    private def eigensystem(m: Array[Array[Int]]) = {
      val Seq(eigenvectors, diagonal) = Eigen.eigenvectors(new DoubleMatrix(m.map(_.map(_.toDouble)))).toSeq
      val eigenvalues = {
        val d = diagonal.toArray2
        for (i <- 0 until m.length) yield d(i)(i)
      }
      (eigenvalues, eigenvectors.toArray2.transpose.toIndexedSeq)
    }

    case class MatrixWithEigendata(m: Array[Array[Int]]) {
      lazy val eigenspaces = {

        val preliminaryEigenspaces = (for ((e, x) <- eigensystem(m).zipped) yield Eigenspace(e, Seq(x))).toSeq
        def mergeEigenspaces(eigenspaces: Seq[Eigenspace]): Seq[Eigenspace] = {
          (for (((e, i), tails) <- eigenspaces.iterator.zipWithIndex.zip(eigenspaces.tails.toSeq.tail.iterator); (f, j) <- tails.zipWithIndex; if (e.eigenvalue.add(f.eigenvalue.neg)).abs < eigenvalueTolerance) yield {
            (i, i + j + 1)
          }).toStream.headOption match {
            case None => eigenspaces
            case Some((i, j)) => mergeEigenspaces(eigenspaces.updated(i, Eigenspace(eigenspaces(i).eigenvalue, eigenspaces(i).eigenbasis ++ eigenspaces(j).eigenbasis)).take(j) ++ eigenspaces.drop(j + 1))
          }
        }
        val result = mergeEigenspaces(preliminaryEigenspaces).toIndexedSeq
        require(result.map(_.eigenbasis.size).sum == m.length)
        result
      }
      lazy val diagonalisation = {
        if (eigenspaces.forall(_.eigenbasis.size == 1)) {
          Some(DiagonalisedMatrix(m, eigenspaces.map(_.normalise.eigenbasis.head).toArray))
        } else {
          None
        }
      }
    }

    case class DiagonalisedMatrix(m: Array[Array[Int]], eigenvectors: Array[Array[ComplexDouble]]) {
      lazy val symmetrised: Option[CandidateSMatrix] = {
          symmetrise(eigenvectors) match {
            case None => None
            case Some(s) => Some(CandidateSMatrix(s))
          }
      }
    }
    
    def rowShufflesToSymmetric[A](m: Array[Array[A]], sameTest: (A, A) => Boolean): Iterator[Array[Array[A]]] = {
      def step(k: Int, m: Array[Array[A]]): Iterator[Array[Array[A]]] = {
        if (k == m.length) {
          Iterator(m)
        } else {
          Iterator.range(k, m.length).filter({ t =>
            (0 until k).forall({ j => sameTest(m(t)(j), m(j)(k)) })
          }).map({ t =>
            val n = m.clone
            n(t) = m(k)
            n(k) = m(t)
            n
          })
        }
      }
      (0 until m.length).foldLeft(Iterator(m))({ case (i, k) => i.flatMap(m => step(k, m)) })
    }

    def symmetrise(m: Array[Array[ComplexDouble]]): Option[Array[Array[ComplexDouble]]] = {
      rowShufflesToSymmetric[ComplexDouble](m, { case (x, y) => x.add(y.neg).abs < 0.001 }).toStream.headOption
    }

    case class CandidateSMatrix(s: Array[Array[ComplexDouble]]) {
      lazy val verlindeMultiplicities: Option[Array[Array[Array[Int]]]] = {
        val rank = s.length

        val NOptions = Array.tabulate(rank, rank, rank)({ (i, j, k) =>
          val x = (for (l <- 0 until rank) yield s(l)(j).mul(s(l)(i)).mul(s(l)(k).conj).div(s(l)(0))).reduce(_.add(_))
          if (x.imag.abs < 0.001 && x.real - x.real.round < 0.001 && x.real.round >= 0) Some(x.real.round.toInt) else None
        })

        if (NOptions.forall(_.forall(_.forall(_.nonEmpty)))) {
          // shall we do some more checking here?

          // are they compatible with the specified orbit structure?
          // are they compatible with the specified matrices?
          val N = NOptions.map(_.map(_.map(_.get)))

          def associative_?(N: Array[Array[Array[Int]]]) = {
            (for (i <- (0 until rank); j <- 0 until rank; k <- 0 until rank; l <- 0 until rank) yield {
              Sum(rank)({ m => N(i)(k)(m) * N(j)(m)(l) - N(i)(j)(m) * N(m)(k)(l) })
            }).forall(_ == 0)
          }

          if (associative_?(N)) {
            Some(N)
          } else {
            None
          }
        } else {
          None
        }
      }

    }

    def firstNonInvertibleObjectMatricesWithEigendata = firstNonInvertibleObjectMatrices.map(MatrixWithEigendata)
  }
}

case class SymmetricMatrixEnumerator(
    initialMatrix: Array[Array[Int]],
    eigenvalueBound: Double,
    coefficientClusters: Array[List[(Int, Int)]]) extends Iterator[Array[Array[Int]]] {

  val rank = initialMatrix.length
  val steps = coefficientClusters.length
  var hint = Array.fill(rank)(1.0)

  // TODO combine these?
  var stack = initialMatrix :: Nil
  var state = -1 :: Nil
  var currentCluster = 0

  private def updateMatrix(m: Array[Array[Int]], cluster: Int, entry: Int): Option[Array[Array[Int]]] = {
    if (entry == 0) {
      Some(m)
    } else {
      val result = m.clone
      for ((i, j) <- coefficientClusters(cluster)) {
        result(i) = result(i).clone
        result(i)(j) = entry
      }
      val estimate = FrobeniusPerronEigenvalues.estimateWithEigenvector(result, 0.0001, eigenvalueBound, Some(hint))
      if (estimate._1 < eigenvalueBound) {
        hint = estimate._2
        Some(result)
      } else {
        None
      }
    }
  }

  @scala.annotation.tailrec
  override final def hasNext: Boolean = {
    if (stack.isEmpty) {
      false
    } else {
      if (currentCluster == steps) {
        true
      } else {
        state = (state.head + 1) :: state.tail
        updateMatrix(stack.head, currentCluster, state.head) match {
          case Some(next) => {
            stack = next :: stack
            state = -1 :: state
            currentCluster = currentCluster + 1
            hasNext
          }
          case None => {
            stack = stack.tail
            state = state.tail
            currentCluster = currentCluster - 1
            hasNext
          }
        }
      }
    }
  }
  override def next: Array[Array[Int]] = {
    val result = stack.head
    stack = stack.tail
    state = state.tail
    currentCluster = currentCluster - 1
    result
  }

}