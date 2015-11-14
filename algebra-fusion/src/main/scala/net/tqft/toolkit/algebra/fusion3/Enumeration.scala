package net.tqft.toolkit.algebra.fusion3

import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger
import net.tqft.toolkit.functions.Memo
import scala.collection.mutable.ListBuffer
import java.util.concurrent.ConcurrentHashMap
import com.google.common.cache.CacheBuilder
import com.google.common.cache.CacheLoader
import net.tqft.toolkit.permutations.Permutations
import net.tqft.toolkit.algebra.graphs.Graph
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import org.jblas.Eigen
import org.jblas.DoubleMatrix

// ideas:
// * replace quadratics so we keep a list of nonzero terms, not a long list of 1/0/-1
// * can we sometimes solve ahead a bit? it doesn't matter for zeros, but propagrating nonzero values forward helps with
//   eigenvalue estimates
// * minimum dimension for each object?
// * can we do other problems? find fusion ring of centre, given the base ring and the induction matrix?

case class Enumeration(selfDualObjects: Int, dualPairs: Int, globalDimensionBound: Double, umtc: Boolean) {
  val rank = selfDualObjects + 2 * dualPairs

  private val dualData = ((0 until selfDualObjects) ++ (for (i <- 0 until dualPairs; n <- Seq(selfDualObjects + 2 * i + 1, selfDualObjects + 2 * i)) yield n)).toIndexedSeq

  private val multiplicities = for (i <- 1 until rank; j <- 1 until rank; k <- 1 until rank) yield Seq(i, j, k)

  private val ordering: Ordering[Seq[Int]] = {
    import Ordering.Implicits._
    import net.tqft.toolkit.orderings.Orderings._

    val lexicographic: Ordering[Seq[Int]] = implicitly

    Ordering.by({ x: Seq[Int] => x.min }).refineAlong(lexicographic)

  }

  private def minReciprocal(v: Seq[Int]) = {
    v match {
      case Seq(i, j, k) => {
        val reciprocals0 = Seq(Seq(i, j, k), Seq(dualData(i), k, j), Seq(j, dualData(k), dualData(i)), Seq(dualData(j), dualData(i), dualData(k)), Seq(dualData(k), i, dualData(j)), Seq(k, dualData(j), i))
        val reciprocals = if (umtc) {
          reciprocals0 ++ reciprocals0.map({ case Seq(i, j, k) => Seq(j, i, k) })
        } else {
          reciprocals0
        }
        reciprocals.min(ordering)
      }
    }
  }
  private val representativeMultiplicities = {
    multiplicities.filter(m => m == minReciprocal(m)).sorted(ordering)
  }
  println(representativeMultiplicities)

  private val numberOfVariables = representativeMultiplicities.size
  private val lookup = {
    val l0 = for (m <- multiplicities) yield representativeMultiplicities.indexOf(minReciprocal(m))
    def lookup(i: Int, j: Int, k: Int): Either[Int, Int] = {
      if (i == 0) {
        if (j == k) Left(1) else Left(0)
      } else {
        if (j == 0) {
          if (i == k) Left(1) else Left(0)
        } else {
          if (k == 0) {
            if (i == dualData(j)) Left(1) else Left(0)
          } else {
            Right(l0((i - 1) * (rank - 1) * (rank - 1) + (j - 1) * (rank - 1) + (k - 1)))
          }
        }
      }
    }
    Array.tabulate(rank, rank, rank)(lookup)
  }

  private val objectFinishedAtStep = {
    (for (i <- 1 until rank) yield (multiplicities.filter(_.min == i).map({ case Seq(i, j, k) => lookup(i)(j)(k).right.get }).max) -> i).toMap
  }
  //  println(lastStepForObject)

  private def N(x: Array[Int])(i: Int, j: Int, k: Int) = {
    lookup(i)(j)(k) match {
      case Left(m) => m
      case Right(z) => x(z)
    }
  }

  lazy val root = {
    val zeroes = Array.fill(numberOfVariables)(0)
    val r = {
      Array.tabulate(rank, rank)({ (i, j) =>
        var t = 0
        for ((k, l) <- Rterms(0)(i)(j)) {
          t = t + N(zeroes)(k, j, l) * N(zeroes)(dualData(k), l, i)
        }
        t
      })
    }
    Partial(-1, zeroes, Nil, r, Some(AssociativityData.root), Array.fill(rank)(1.0), None)
  }

  private val unlabelledGraphs = {
    IndexedSeq.tabulate(rank + 1)({ r =>
      val adjacencies = {
        // note here we decorate with dual data
        IndexedSeq.tabulate(r)(i => Seq(i + r, dualData(i))) ++
          IndexedSeq.tabulate(r)(i => Seq(i + 2 * r)) ++
          IndexedSeq.tabulate(r)(i => Seq(i)) ++
          IndexedSeq.tabulate(r, r, r)({
            case (i, j, k) => Seq(i, j + r, k + 2 * r)
          }).map(_.flatten).flatten
      }
      val numberOfVertices = 3 * r + r * r * r
      Graph(numberOfVertices, adjacencies)
        .colour(IndexedSeq(0).take(r).padTo(numberOfVertices, 1)) // color the identity
        .combineColours((IndexedSeq.fill(r)(-3) ++ IndexedSeq.fill(r)(-2) ++ IndexedSeq.fill(r)(-1)).padTo(numberOfVertices, 0)) // color the triangles
    })
  }

  case class Complete(rank: Int, m: Seq[Seq[Seq[Int]]], r: Seq[Seq[Int]], globalDimensionEstimate: Double) {
    val S = {
      val Seq(eigenvectors, diagonal) = Eigen.symmetricEigenvectors(new DoubleMatrix(r.map(_.map(_.toDouble).toArray).toArray)).toSeq
      val eigenvalues = {
        val d = diagonal.toArray2
        for (i <- 0 until rank) yield d(i)(i)
      }
      //      println((eigenvalues, r.map(_.mkString("{", ",", "}")).mkString("{", ",", "}"), eigenvectors.mmul(eigenvectors.transpose)))
      eigenvectors
    }
    val umtcCompatible_? = {

    }

    override def toString = {
      val max = m.map(_.map(_.max)).map(_.max).max
      val sep = if (max > 9) "," else ""
      selfDualObjects + "," + dualPairs + " " + max + " " + m.map(_.flatten).flatten.mkString(sep) + " " + globalDimensionEstimate
    }

    lazy val dualData: Seq[Int] = {
      for (i <- 0 until rank) yield {
        m(i).map(_.head).indexOf(1)
      }
    }

    def permute(p: IndexedSeq[Int]): Complete = {
      require(p(0) == 0)
      Complete(rank,
        IndexedSeq.tabulate(rank, rank, rank)({ (i, j, k) => m(p(i))(p(j))(p(k)) }),
        IndexedSeq.tabulate(rank, rank)({ (i, j) => r(p(i))(p(j)) }),
        globalDimensionEstimate)
    }

    lazy val graphPresentation = {
      val colours = IndexedSeq.fill(3 * rank)(-1) ++ m.map(_.flatten).flatten
      unlabelledGraphs(rank).combineColours(colours)
    }
    lazy val automorphisms: net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = {
      FiniteGroups.symmetricGroup(rank).subgroupGeneratedBy(graphPresentation.automorphismGroup.generators.map(_.take(rank)))
    }

    lazy val canonicalize: Complete = {
      val result = permute(Dreadnaut.canonicalLabelling(graphPresentation).take(rank))
      result
    }
  }

  object Partial {
    def apply(s: String): Partial = {
      val numbers = if (s.contains(",")) {
        s.split(',').map(_.toInt)
      } else {
        s.toCharArray().map(_.toString.toInt)
      }
      numbers.foldLeft(root)(_.next(_).get)
    }
  }

  private def numericallyDistinct(z: Seq[Double]) = {
    z.sliding(2).forall({ p => scala.math.abs(p(0) - p(1)) > 0.001 })
  }

  def eigensystem(m: Array[Array[Double]]) = {
    val Seq(eigenvectors, diagonal) = Eigen.symmetricEigenvectors(new DoubleMatrix(m.map(_.toArray).toArray)).toSeq
    val eigenvalues = {
      val d = diagonal.toArray2
      for (i <- 0 until rank) yield d(i)(i)
    }
    (eigenvalues, eigenvectors.toArray2)
  }

  case class Partial(
      step: Int,
      x: Array[Int],
      zeroes: List[Int],
      r: Array[Array[Int]],
      associativity: Option[AssociativityData],
      eigenvectorHint: Array[Double],
      umtcHint: Option[Array[Array[Double]]]) {

    override def toString = {
      val sep = if (x.max > 9) "," else ""
      x.take(step + 1).mkString(sep)
    }

    def complete: Option[Complete] = {
      if (done_?) {
        val m = Seq.tabulate(rank, rank, rank)({ (i, j, k) => N(x)(i, j, k) })
        Some(Complete(rank, m, r.map(_.toSeq).toSeq, FrobeniusPerronEigenvalues.estimate(r)))
      } else {
        None
      }
    }

    def umtcFastForward: Option[Partial] = {
      if (umtc) {
        objectFinishedAtStep.get(step) match {
          case None => {
            Some(this)
          }
          case Some(i0) => {
            val m = {
              if (i0 == 1) {
                Array.tabulate(rank, rank)({ (j, k) => N(x)(1, j, k).toDouble })
              } else {
                val hint = umtcHint.get
                Array.tabulate(rank, rank)({ (j, k) => hint(j)(k) * scala.math.Pi / 3 + N(x)(i0, j, k) })
              }
            }
            val (eigenvalues, s) = eigensystem(m)
            if (numericallyDistinct(eigenvalues)) {
              //              println("wow, distinct eigenvalues at step " + step)
              val NN = Array.tabulate(rank, rank, rank)({ (i, j, k) =>
                val x = (for (l <- 0 until rank) yield s(j)(l) * s(i)(l) * s(k)(l) / s(0)(l)).sum
                if (x - scala.math.round(x) < 0.01 && scala.math.round(x) >= 0) Some(scala.math.round(x).toInt) else None
              })
              if (NN.forall(_.forall(_.forall(_.nonEmpty)))) {
                val nextSteps = for (Seq(i, j, k) <- representativeMultiplicities.drop(step + 1)) yield NN(i)(j)(k).get
                nextSteps.foldLeft[Option[Partial]](Some(this))({ case (o, m) => o.flatMap(_.next(m)).filter(_.associativity.nonEmpty) }).ensuring(_.forall(_.done_?))
              } else {
                None
              }
            } else {
              Some(this.copy(umtcHint = Some(m)))
            }
          }
        }
      } else {
        Some(this)
      }
    }

    def next(m: Int): Option[Partial] = {
      val nextX = x.clone()
      nextX(step + 1) = m

      if (inequalities(step + 1, nextX)) {
        val nextAssociativity = associativity.flatMap(_.evaluateNextVariable(m))

        if (m == 0) {
          Some(Partial(step + 1, x, (step + 1) :: zeroes, r, nextAssociativity, eigenvectorHint, umtcHint))
        } else {

          val nextR = Array.tabulate(rank, rank)({ (i, j) =>
            var t = r(i)(j)
            for ((k, l) <- Rterms(step + 2)(i)(j)) {
              t = t + N(nextX)(k, j, l) * N(nextX)(dualData(k), l, i)
            }
            t
          })

          val (eigenvalue, nextEigenvectorHint) = FrobeniusPerronEigenvalues.estimateWithEigenvector(nextR, 0.001, globalDimensionBound + 1, Some(eigenvectorHint))

          if (eigenvalue <= globalDimensionBound) {
            Some(Partial(step + 1, nextX, zeroes, nextR, nextAssociativity, nextEigenvectorHint, umtcHint))
          } else {
            None
          }
        }
      } else {
        None
      }
    }

    def children = {
      Iterator.from(0).map(next).takeWhile(_.nonEmpty).map(_.get).filter(_.associativity.nonEmpty).flatMap(_.umtcFastForward).toSeq
    }

    def done_? = step == numberOfVariables - 1

    def descendants: Seq[Complete] = {
      if (done_?) {
        complete.toSeq
      } else {
        children.par.flatMap(_.descendants).seq
      }
    }

    def interruptibleDescendants(notify: Complete => Unit = { p => () }): (Future[(Seq[Partial], Seq[Complete])], () => Unit) = {
      val latch = new AtomicInteger(1)
      def interrupt {
        latch.decrementAndGet
      }

      import scala.concurrent.ExecutionContext.Implicits.global

      (Future { interruptibleDescendantsWorker(latch, notify) }, interrupt _)
    }

    private def interruptibleDescendantsWorker(latch: AtomicInteger, notify: Complete => Unit): (Seq[Partial], Seq[Complete]) = {
      if (latch.get > 0) {
        //        println("latch open, working on " + this)
        complete match {
          case Some(c) => {
            notify(c)
            (Seq.empty, Seq(c))
          }
          case None => {
            val results = children.par.map(_.interruptibleDescendantsWorker(latch, notify)).seq
            (results.flatMap(_._1), results.flatMap(_._2))
          }
        }
      } else {
        //        println("latch closed, reporting " + this)
        (Seq(this), Seq.empty)
      }
    }

  }

  private def inequalities(step: Int, x: Array[Int]): Boolean = {
    //    true

    // all we worry about is that the diagonal entries are descending (or as descending as we can make them, given that we have the dual pairs together, at the end)

    val v = representativeMultiplicities(step)
    if (v(0) > 1 && v(0) == v(1) && v(1) == v(2)) {
      if (v(0) < selfDualObjects || (v(0) - selfDualObjects) % 2 == 1) {
        val pstep = lookup(v(0) - 1)(v(0) - 1)(v(0) - 1).right.get
        require(pstep < step)
        //        println((step, pstep, v(0), v(0)-1, lookup(v(0) - 1)(v(0) - 1)(v(0) - 1)))
        x(step) == x(pstep) || {
          x(step) < x(pstep) && {
            true
            // try require canonical form! --- very slow??
            //            val f = (for (i <- 1 to v(0) - 1; if (x(lookup(i)(i)(i).right.get)) == x(pstep)) yield i).head
            //            v(0) <= f + 1 || {
            //              val p = canonicalPermutation(x, v(0) - 1, f - 1)
            //              p == p.sorted
            //            }
          }
        }
      } else if (v(0) > selfDualObjects && (v(0) - selfDualObjects) % 2 == 0) {
        val pstep = lookup(v(0) - 2)(v(0) - 2)(v(0) - 2).right.get
        require(pstep < step)
        x(step) <= x(pstep)
      } else {
        require(v(0) == selfDualObjects)
        true
      }
    } else {
      true
    }
  }

  private val Rterms = {
    import net.tqft.toolkit.arithmetic.MinMax._

    val terms = for (i <- 0 until rank; j <- 0 until rank; k <- 0 until rank; l <- 0 until rank) yield {
      (i, j, (k, l), Seq(lookup(k)(j)(l), lookup(dualData(k))(l)(i)).collect({ case Right(z) => z }).maxOption.getOrElse(-1))
    }

    val map = terms.groupBy(_._4).mapValues(_.groupBy(_._1).mapValues(_.groupBy(_._2)))

    IndexedSeq.tabulate(numberOfVariables + 1, rank, rank)({ (s, i, j) =>
      map.get(s - 1) match {
        case None => IndexedSeq.empty
        case Some(map) => map.get(i) match {
          case None => IndexedSeq.empty
          case Some(map) => map.getOrElse(j, IndexedSeq.empty).map(_._3)
        }
      }
    })
  }

  object AssociativityData {
    val root: AssociativityData = {
      val polynomials = (for (a <- 1 until rank; b <- a until rank; c <- a until rank; d <- a until rank; p <- apply(a, b, c, d)) yield p).distinct
      println("AssociativityData.root:")
      for (p <- polynomials) println("   " + p)
      AssociativityData(polynomials)
    }

    def apply(a: Int, b: Int, c: Int, d: Int): Option[PolynomialPair] = {
      var constant = 0
      var linear = Array.fill(numberOfVariables)(0)
      var quadratic = Array.fill(numberOfVariables, numberOfVariables)(0)

      for (e <- (0 until rank)) {
        (lookup(a)(b)(e), lookup(e)(c)(d)) match {
          case (Left(1), Left(1)) => constant = constant + 1
          case (Left(0), _) =>
          case (_, Left(0)) =>
          case (Left(1), Right(i)) => linear(i) = linear(i) + 1
          case (Right(i), Left(1)) => linear(i) = linear(i) + 1
          case (Right(i), Right(j)) if i < j => quadratic(j)(i) = quadratic(j)(i) + 1
          case (Right(i), Right(j)) if i >= j => quadratic(i)(j) = quadratic(i)(j) + 1
        }
        (lookup(a)(e)(d), lookup(b)(c)(e)) match {
          case (Left(1), Left(1)) => constant = constant - 1
          case (Left(0), _) =>
          case (_, Left(0)) =>
          case (Left(1), Right(i)) => linear(i) = linear(i) - 1
          case (Right(i), Left(1)) => linear(i) = linear(i) - 1
          case (Right(i), Right(j)) if i < j => quadratic(j)(i) = quadratic(j)(i) - 1
          case (Right(i), Right(j)) if i >= j => quadratic(i)(j) = quadratic(i)(j) - 1
        }
      }
      println(constant)
      println(linear.toList)
      println(quadratic.toList.map(_.toList))
      val pair = PolynomialPair(constant, linear, quadratic)
      println(pair)
      if (pair.identicallyZero_?) {
        None
      } else {
        Some(pair)
      }
    }
  }

  case class AssociativityData(equations: Seq[PolynomialPair]) {
    def evaluateNextVariable(a: Int): Option[AssociativityData] = {
      Some(AssociativityData(for (e <- equations) yield {
        e.evaluateNextVariable(a) match {
          case None => return None
          case Some(f) => f
        }
      }))
    }
  }

  object PolynomialPair {
    /* quadratic should be lower triangular */
    def apply(constant: Int, linear: Array[Int], quadratic: Array[Array[Int]]): PolynomialPair = {
      val lhsLinearTerms = linear.toList.map({ x => if (x >= 0) x else 0 })
      val rhsLinearTerms = linear.toList.map({ x => if (x < 0) x else 0 })
      val columns = for ((column, i) <- quadratic.transpose.toList.zipWithIndex) yield column.drop(i)
      val quadraticTerms = for (column <- columns) yield {
        (column.head, column.tail.toList.zipWithIndex.collect[Either[Int, Int], List[Either[Int, Int]]]({
          case (x, i) if x > 0 => {
            require(x == 1)
            Left(i)
          }
          case (x, i) if x < 0 => {
            require(x == -1)
            Right(i)
          }
        }))
      }
      val numberOfLhsQuadraticTerms = quadraticTerms.lastIndexWhere({ p =>
        p._1 > 0 || p._2.exists(_.isLeft)
      }) + 1
      val numberOfRhsQuadraticTerms = quadraticTerms.lastIndexWhere({ p =>
        p._1 < 0 || p._2.exists(_.isRight)
      }) + 1
      if (numberOfLhsQuadraticTerms >= numberOfRhsQuadraticTerms) {
        if (numberOfLhsQuadraticTerms == 0) {
          LinearPair(constant, lhsLinearTerms, rhsLinearTerms)
        } else {
          QuadraticPair(LinearPair(constant, lhsLinearTerms, rhsLinearTerms), quadraticTerms.take(numberOfLhsQuadraticTerms), numberOfLhsQuadraticTerms, numberOfRhsQuadraticTerms)
        }
      } else {
        val swappedQuadraticTerms: List[(Int, List[Either[Int, Int]])] = for ((d, e) <- quadraticTerms) yield {
          (-d, e.map(_.swap))
        }
        QuadraticPair(LinearPair(-constant, rhsLinearTerms, lhsLinearTerms), swappedQuadraticTerms.take(numberOfRhsQuadraticTerms), numberOfRhsQuadraticTerms, numberOfLhsQuadraticTerms)
      }
    }
  }

  sealed trait PolynomialPair {
    def evaluateNextVariable(a: Int): Option[PolynomialPair]
    def identicallyZero_? : Boolean
  }

  case class LinearPair(constant: Int, lhsLinearTerms: List[Int], rhsLinearTerms: List[Int]) extends PolynomialPair {
    def definitelyPositive_? = constant > 0 && rhsLinearTerms.isEmpty
    override def identicallyZero_? = constant == 0 && lhsLinearTerms.forall(_ == 0) && rhsLinearTerms.forall(_ == 0)

    override def evaluateNextVariable(a: Int): Option[LinearPair] = {
      val newConstant = constant + (lhsLinearTerms.head - rhsLinearTerms.head) * a
      if ((newConstant > 0 && (rhsLinearTerms.isEmpty || rhsLinearTerms.tail.isEmpty)) || (newConstant < 0 && (lhsLinearTerms.isEmpty || lhsLinearTerms.tail.isEmpty))) {
        None
      } else {
        Some(LinearPair(newConstant, lhsLinearTerms.drop(1), rhsLinearTerms.drop(1)))
      }
    }
  }
  /* quadraticTerms contains -1 if the term appears on the lhs, 0 if it doesn't appear at all, -1 if it appears on the rhs */
  /*
   *  Each entry of quadraticTerms is an (Int, List[Int]).
   *   The first Int is either 1, 0, or -1, depending on whether there is an x_i^2 on the left side, neither side, or the right side.
   *   The List[Int] then contains Left(j-i-1) if there is an x_i x_j term on the left side, or Right(j-i-1) if there is on the right side. 
   */
  case class QuadraticPair(linearPair: LinearPair, quadraticTerms: List[(Int, List[Either[Int, Int]])], numberOfLhsQuadraticTerms: Int, numberOfRhsQuadraticTerms: Int) extends PolynomialPair {
    require(numberOfLhsQuadraticTerms >= 1)
    require(numberOfRhsQuadraticTerms <= numberOfLhsQuadraticTerms)
    require(quadraticTerms.size == numberOfLhsQuadraticTerms)

    override def identicallyZero_? = linearPair.identicallyZero_? && quadraticTerms.forall({ p => p._1 == 0 && p._2.isEmpty })

    override def evaluateNextVariable(a: Int): Option[PolynomialPair] = {
      if (a == 0) {
        val newLinearPair = LinearPair(linearPair.constant, linearPair.lhsLinearTerms.tail, linearPair.rhsLinearTerms.tail)
        if (numberOfLhsQuadraticTerms == 1) {
          Some(newLinearPair)
        } else {
          Some(QuadraticPair(newLinearPair, quadraticTerms.tail, numberOfLhsQuadraticTerms - 1, numberOfRhsQuadraticTerms - 1))
        }
      } else {
        val newConstant = linearPair.constant + ((linearPair.lhsLinearTerms.head - linearPair.rhsLinearTerms.head) + quadraticTerms.head._1 * a) * a

        val newLinearPair = {
          val lhs = linearPair.lhsLinearTerms.tail.toArray
          val rhs = linearPair.rhsLinearTerms.tail.toArray

          for (x <- quadraticTerms.head._2) {
            x match {
              case Left(i) => {
                if (lhs(i) == 0) {
                  if (rhs(i) < a) {
                    lhs(i) = a - rhs(i)
                    rhs(i) = 0
                  } else {
                    rhs(i) = rhs(i) - a
                  }
                } else {
                  lhs(i) = lhs(i) + a
                }
              }
              case Right(i) => {
                if (rhs(i) == 0) {
                  if (lhs(i) < a) {
                    rhs(i) = a - lhs(i)
                    lhs(i) = 0
                  } else {
                    lhs(i) = lhs(i) - a
                  }
                } else {
                  rhs(i) = rhs(i) + a
                }
              }
            }
          }
          // TODO is switching back and forth between Arrays and Lists too expensive here? We can probably stick with Arrays.
          LinearPair(newConstant, lhs.toList, rhs.toList)
        }

        if (numberOfLhsQuadraticTerms > 1) {
          if (numberOfRhsQuadraticTerms > 1 || !newLinearPair.definitelyPositive_?) {
            Some(QuadraticPair(newLinearPair, quadraticTerms.tail, numberOfLhsQuadraticTerms - 1, numberOfRhsQuadraticTerms - 1))
          } else {
            None
          }
        } else {
          if (newLinearPair.definitelyPositive_?) {
            None
          } else {
            Some(newLinearPair)
          }
        }
      }
    }
  }

}
  
