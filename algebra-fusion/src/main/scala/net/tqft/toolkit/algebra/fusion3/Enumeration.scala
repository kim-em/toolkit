package net.tqft.toolkit.algebra.fusion3

import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues
import net.tqft.toolkit.arithmetic.Sum
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
import org.jblas.ComplexDouble

case class Enumeration(
    selfDualObjects: Int,
    dualPairs: Int,
    globalDimensionBound: Double,
    umtc: Boolean,
    minimumDimension: Option[Double],
    withFunctor: Option[(Array[Array[Int]], Array[Array[Array[Int]]])],
    withMatrix: Option[Array[Array[Int]]]) {
  val rank = selfDualObjects + 2 * dualPairs

  private val dualData = ((0 until selfDualObjects) ++ (for (i <- 0 until dualPairs; n <- Seq(selfDualObjects + 2 * i + 1, selfDualObjects + 2 * i)) yield n)).toIndexedSeq

  private val multiplicities = for (i <- 1 until rank; j <- 1 until rank; k <- 1 until rank) yield Seq(i, j, k)

  private val checkTallies = scala.collection.mutable.Map[Int, Int]()

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
  //  println("representativeMultiplicities = " + representativeMultiplicities)

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
    (for (i <- 1 until rank) yield (multiplicities.filter(_.contains(i)).map({ case Seq(i, j, k) => lookup(i)(j)(k).right.get }).max) -> i).toMap
  }
  //  println("objectFinishedAtStep = " + objectFinishedAtStep)

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
    // TODO remove this, no need to keep track of the identity
    val id = Array.tabulate(rank, rank)({ (i, j) => if (i == j) 1 else 0 })
    val empty = Partial(-1, zeroes, Nil, r, Array.fill(rank)(1.0), None, IndexedSeq(id), 0.0)
    withMatrix match {
      case None => empty
      case Some(matrix) => {
        require(selfDualObjects > 0 && matrix(1)(0) == 1 || selfDualObjects == 0 && matrix(1)(0) == 0 && matrix(2)(0) == 1)

        val nextSteps = representativeMultiplicities.takeWhile(_.contains(1)).map({ v =>
          require(v.head == 1)
          matrix(v(1))(v(2))
        })
        nextSteps.foldLeft[Option[Partial]](Some(empty))({ case (o, m) => o.flatMap(_.next(m)).flatMap(_.associative_?) }) match {
          case None => {
            //            println("No root compatible with the specified initial matrix.")
            //            println(matrix.map(_.mkString).mkString("\n"))
            throw new NoSuchElementException
          }
          case Some(r) => r
        }
      }
    }
  }

  def verify(N: String): Boolean = {
    if (N.size == rank * rank * rank) {
      verify(N.sliding(rank * rank, rank * rank).map(_.sliding(rank, rank).map(_.toCharArray.map(_.toString.toInt).toList).toList).toList)
    } else {
      ???
    }
  }

  def verify(N: Seq[Seq[Seq[Int]]]): Boolean = {
    val nextSteps = for (Seq(i, j, k) <- representativeMultiplicities) yield N(i)(j)(k)
    nextSteps.foldLeft[Option[Partial]](Some(root))({
      case (o, m) => {
        //        println("---")
        //        println(o)
        //        println(m)
        val n = o.flatMap(_.next(m))
        //        println(n)
        val a = n.flatMap(_.associative_?)
        //        println(a)
        a
      }
    }).ensuring(_.forall(_.done_?)).nonEmpty
  }

  private val unlabelledGraphs = {
    IndexedSeq.tabulate(rank + 1)({ r =>
      val adjacencies = {
        // note here we decorate with dual data
        IndexedSeq.tabulate(r)(i => Seq(i + r) ++ (if (dualData(i) != i) Seq(dualData(i)) else Nil)) ++
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

  private def numericallyDistinctIndices(z: Seq[ComplexDouble]): Seq[Int] = {
    for ((w, i) <- z.zipWithIndex; if z.count(y => y.add(w.neg).abs < 0.001) == 1) yield i
  }

  private def eigensystem(m: Array[Array[Double]]) = {
    val Seq(eigenvectors, diagonal) = Eigen.eigenvectors(new DoubleMatrix(m.map(_.toArray).toArray)).toSeq
    val eigenvalues = {
      val d = diagonal.toArray2
      for (i <- 0 until rank) yield d(i)(i)
    }
    (eigenvalues, eigenvectors.toArray2.transpose)
  }

  private val maximumAtStep: Array[Int] = {
    if (withFunctor.nonEmpty) {
      val functor = withFunctor.get._1
      val targetRing = withFunctor.get._2
      val targetRank = targetRing.length

      require(functor.length == targetRank)
      require(functor.forall(_.length == rank))
      require(targetRing.length == targetRank)
      require(targetRing.forall(_.length == targetRank))
      require(targetRing.forall(_.forall(_.length == targetRank)))

      Array.tabulate(numberOfVariables)({ step =>
        val Seq(i, j, k) = representativeMultiplicities(step)
        val m = (for (c <- 0 until targetRank; if functor(c)(k) > 0) yield {
          ((for (d <- 0 until targetRank; e <- 0 until targetRank) yield {
            functor(d)(i) * functor(e)(j) * targetRing(d)(e)(c)
          }).sum.toDouble / functor(c)(k) + 0.01).floor.toInt
        }).min
        Seq(m, ((scala.math.sqrt(globalDimensionBound) + 0.01).floor.toInt)).min
      })
    } else {
      Array.fill(numberOfVariables)((scala.math.sqrt(globalDimensionBound) + 0.01).floor.toInt)
    }
  }

  case class Partial(
      step: Int,
      x: Array[Int],
      zeroes: List[Int],
      r: Array[Array[Int]],
      hint: Array[Double],
      umtcHint: Option[Array[Array[Double]]],
      finishedMatrices: IndexedSeq[Array[Array[Int]]],
      finishedDimensions: Double) {

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

    def checkMatrix(i0: Int, actually: Boolean): Option[Partial] = {
      //      println(s"checkMatrix($i0), with step = $step and x = ${x.toSeq}")

      // TODO: to think about --- do we need to separately check that the eigenvectors of our generic linear combo are eigenvectors of the latest matrix?
      lazy val n = Array.tabulate(rank, rank)({ (j, k) => N(x)(i0, j, k) })
      lazy val newFinishedMatrices = finishedMatrices :+ n

      val m = {
        if (i0 == 1) {
          Array.tabulate(rank, rank)({ (j, k) => n(j)(k).toDouble })
        } else {
          val hint = umtcHint.get
          Array.tabulate(rank, rank)({ (j, k) => hint(j)(k) * scala.math.Pi / 3 + n(j)(k) })
        }
      }
      //      if(!done_?) {
      //      checkTallies(i0) = checkTallies.getOrElse(i0, 0) + 1
      //      if(checkTallies(i0) % 100 == 0) {
      //      println(checkTallies)
      //      }
      //      }
      val (eigenvalues, s0) = eigensystem(m)
      //      println("m: " + m.toList.map(_.toList))

      //      println("s: " + s.toList.map(_.toList))
      val s = s0.map({ v =>
        val norm = scala.math.sqrt(v.map(x => scala.math.pow(x.abs, 2)).sum)
        val f = v(0).div(v(0).abs).mul(norm)
        v.map(x => x.div(f))
      })

      //            println("i0: " + i0)
      //            println(m.toList.map(_.toList))
      //            println("eigenvalues: " + eigenvalues)
      //            println("s: " + s.toList.map(_.toList))
      val distinct = numericallyDistinctIndices(eigenvalues)
      //            println("distinct: " + distinct)
      //            println("eigenvectors: " + distinct.map(s(_).toList))

      if (distinct.size == rank) {
        //        println("distinct eigenvalues at step " + step)
        //        println("distinct: " + distinct)

        //        println("eigenvectors: " + distinct.map(s(_).mkString("{", ", ", "}")).mkString("{", ", ", "}"))

        val NN = Array.tabulate(rank, rank, rank)({ (i, j, k) =>
          //          val x = (for (l <- 0 until rank) yield s(j)(l).mul(s(i)(l)).mul(s(dualData(k))(l)).div(s(0)(l))).reduce(_.add(_))
          //          val x = (for (l <- 0 until rank) yield s(j)(l).mul(s(i)(l)).mul(s(k)(l).conj).div(s(0)(l))).reduce(_.add(_))
          val x = (for (l <- 0 until rank) yield s(l)(j).mul(s(l)(i)).mul(s(l)(k).conj).div(s(l)(0))).reduce(_.add(_))
          //          println((i, j, k, x))
          if (x.imag.abs < 0.001 && x.real - x.real.round < 0.001 && x.real.round >= 0) Some(x.real.round.toInt) else None
        })

        //        println(NN.toList.map(_.map(_.toList).toList))

        if (NN.forall(_.forall(_.forall(_.nonEmpty)))) {
          val previousSteps = (for (Seq(i, j, k) <- representativeMultiplicities.take(step + 1)) yield NN(i)(j)(k).get)
          if (x.take(step + 1).toSeq != previousSteps) {
            // println("fusion rules incompatible with previous choices")
            None
          } else {

            // we can check here if there is a permutation of the eigenvectors making s symmetric.
            // FIXME can we search permutations more efficiently??
            val sround = s.map(_.toList.map(x => ((x.real * 1000).round, (x.imag * 1000).round))).toList
            val sroundt = sround.transpose
            val permutations = {
              import net.tqft.toolkit.permutations.Permutations._
              Permutations.of(sround.size).filter(p => p.permute(sround.map(r => p.inverse.permute(r))) == sroundt)
            }
            if (true && !permutations.hasNext) {
              //          println("No permutations of the eigenvectors results in a symmetric matrix")
              None
            } else {

              if (done_?) {
                val c = (for (Seq(i, j, k) <- representativeMultiplicities) yield NN(i)(j)(k).get)
                if (c == x.toSeq) {
                  //                println("Verlinde formula looks good!")
                  Some(this.copy(umtcHint = Some(m), finishedMatrices = newFinishedMatrices))
                } else {
                  //                println("The Verlinde formula breaks!")
                  //                println(s"c = $c")
                  //                println(s"x = ${x.toVector}")
                  None
                }
              } else {
                val nextSteps = (for (Seq(i, j, k) <- representativeMultiplicities.drop(step + 1)) yield NN(i)(j)(k).get)
                val this0 = this.copy(umtcHint = Some(m), finishedMatrices = newFinishedMatrices)
                if (actually) {
                  nextSteps.foldLeft[Option[Partial]](Some(this0))({ case (o, m) => o.flatMap(_.next(m)).flatMap(_.associative_?).flatMap(_.umtcFastForward(false)) }).ensuring(_.forall(_.done_?))
                } else {
                  Some(this0)
                }
              }
            }
          }
        } else {
          //            println("bad fusion rules")
          None
        }
        //        }
      } else {
        // it ain't over yet!

        val eigenvectors = distinct.map(s(_))

        def dot(v: Array[ComplexDouble], w: Array[ComplexDouble]): ComplexDouble = {
          var t = ComplexDouble.ZERO
          for (i <- 0 until v.length) {
            t = t.add(v(i).mul(w(i).conj))
          }
          t
        }
        def dot2(v: Array[ComplexDouble], w: Array[Int]): ComplexDouble = {
          var t = ComplexDouble.ZERO
          for (i <- 0 until v.length) {
            t = t.add(v(i).mul(w(i)))
          }
          t
        }
        def orthogonal(vectors: Seq[Array[ComplexDouble]]): Boolean = {
          for (i <- 0 until vectors.size; j <- i + 1 until vectors.size) {
            if (dot(vectors(i), vectors(j)).abs > 0.001) {
              println((i, j))
              println(dot(vectors(i), vectors(j)).abs)
              for (v <- vectors) println(v.toList.mkString("{", ",", "}"))
              for (m <- finishedMatrices :+ n) println(m.toList.map(_.toList.mkString("{", ",", "}")).mkString("{", ",", "}"))
              require(false)
              return false
            }
          }
          true
        }

        //        // TODO: once things are working, remove the orthogonality check; it should always pass
        //        if (eigenvectors.forall(X => X(0) != ComplexDouble.ZERO) && orthogonal(eigenvectors)) {

        // TODO: as written, this isn't checking anything!
        def d(l: Int, i: Int) = {
          // the eigenvalue of X_i on eigenvector(l)
          val X = newFinishedMatrices(i)
          dot2(eigenvectors(l), X(0)).div(eigenvectors(l)(0))
        }
        //                println("eigenvectors: " + eigenvectors.map(_.toList))
        def check: Boolean = {
          for (l <- 0 until eigenvectors.length; i <- 1 until i0) {
            //                    println("newFinishedMatrices(i): " + newFinishedMatrices(i).toList.map(_.toList))
            //                    println((l,i, d(l,i), eigenvectors(l)(i).div(eigenvectors(l)(0)).conj), d(l, i).add(eigenvectors(l)(i).div(eigenvectors(l)(0)).conj.neg).abs > 0.001)
            if (d(l, i).add(eigenvectors(l)(i).div(eigenvectors(l)(0)).conj.neg).abs > 0.001) return false
          }
          true
        }
        // check that the ratios give the eigenvalues 
        if (check) {
          Some(this.copy(umtcHint = Some(m), finishedMatrices = newFinishedMatrices))
        } else {
          println("bad S matrix")
          None
        }
        //        } else {
        //          if (!eigenvectors.forall(X => X(0) != ComplexDouble.ZERO)) {
        //                        println("bad S matrix: eigenvector starts with a zero")
        //          } else {
        //            require(!orthogonal(eigenvectors))
        //            println("bad S matrix: eigenvectors not orthogonal")
        //          }
        //          None
        //        }
      }

    }

    def umtcFastForward(actually: Boolean): Option[Partial] = {
      if (umtc) {
        objectFinishedAtStep.get(step) match {
          case None => {
            //            println("no objects finished, nothing to check")
            Some(this)
          }
          case Some(i0) => {
            // because sometimes finishing one matrix implies finishing two, we might have to run multiple checks.
            val im = finishedMatrices.size
            //            println("about to check matrices: " + (im to i0))
            (im to i0).foldLeft[Option[Partial]](Some(this))((o, i) => o.flatMap(_.checkMatrix(i, actually)))
          }
        }
      } else {
        Some(this)
      }
    }

    def next(m: Int): Option[Partial] = {

      //      println(this + ".next(" + m + ")")

      val nextX = x.clone()
      nextX(step + 1) = m

      if (inequalities(step + 1, nextX)) {
        if (m == 0) {
          //          println("inequalities look okay")
          Some(Partial(step + 1, x, (step + 1) :: zeroes, r, hint, umtcHint, finishedMatrices, finishedDimensions))
        } else {

          val nextR = Array.tabulate(rank, rank)({ (i, j) =>
            var t = r(i)(j)
            for ((k, l) <- Rterms(step + 2)(i)(j)) {
              t = t + N(nextX)(k, j, l) * N(nextX)(dualData(k), l, i)
            }
            t
          })

          val (eigenvalue, nextHint) = FrobeniusPerronEigenvalues.estimateWithEigenvector(nextR, 0.001, globalDimensionBound + 1, Some(hint))

          if (eigenvalue <= globalDimensionBound) {
            //            println("inequalities and global dimension looks okay")
            Some(Partial(step + 1, nextX, zeroes, nextR, nextHint, umtcHint, finishedMatrices, finishedDimensions))
          } else {
            //            println("rejecting because of global dimension")
            None
          }
        }
      } else {
        //        println("rejecting because of inequalities")
        None
      }
    }

    def XdX(i: Int): Array[Array[Int]] = {
      require(objectFinishedAtStep.get(step) == Some(i))

      //      X_i* X_i X_j = N_ijk X_i* X_k = N_i*kl N_ijk X_l

      Array.tabulate(rank, rank)({ (l, j) => Sum(rank)({ k => N(x)(dualData(i), k, l) * N(x)(i, j, k) }) })
    }
    def objectDimensionAllowed_? : Option[Partial] = {
      minimumDimension match {
        case Some(minimum) => {
          objectFinishedAtStep.get(step) match {
            case None => Some(this)
            case Some(i) => {
              val m = XdX(i)
              val (lowerBound, eigenvectorEstimate) = FrobeniusPerronEigenvalues.estimateWithEigenvector(m)
              if (lowerBound > globalDimensionBound - finishedDimensions - (rank - i - 1) * minimum) {
                None
              } else {
                val upperBound = FrobeniusPerronEigenvalues.upperBound(m, eigenvectorEstimate)
                if (upperBound < minimum) {
                  None
                } else {
                  Some(copy(finishedDimensions = finishedDimensions + lowerBound))
                }
              }
            }
          }
        }
        case None => Some(this)
      }
    }

    def associative_? : Option[Partial] = {
      if (AssociativityData(zeroes).check(step, x)) {
        Some(this)
      } else {
        None
      }
    }

    def children = {
      Iterator
        .from(0)
        .take(maximumAtStep(step + 1) + 1)
        .map(next)
        .takeWhile(_.nonEmpty)
        .map(_.get)
        .flatMap(_.associative_?)
        .flatMap(_.objectDimensionAllowed_?)
        .flatMap(_.umtcFastForward(true))
    }

    def done_? = step == numberOfVariables - 1

    def descendants: Iterator[Complete] = {
      if (done_?) {
        complete.toIterator
      } else {
        children.flatMap(_.descendants)
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
            println(c)
            notify(c)
            (Seq.empty, Seq(c))
          }
          case None => {
            val results = children.toSeq.par.map(_.interruptibleDescendantsWorker(latch, notify)).seq
            (results.flatMap(_._1), results.flatMap(_._2))
          }
        }
      } else {
        //        println("latch closed, reporting " + this)
        (Seq(this), Seq.empty)
      }
    }

  }

  private val withFunctorSymmetryBreaker1 = {
    withFunctor match {
      case None => IndexedSeq.fill(rank)(true)
      case Some((restriction, _)) => {
        val t = restriction.toSeq.map(_.toSeq).transpose
        IndexedSeq.tabulate(rank)({ i =>
          i == 0 || t(i) == t(i - 1)
        })
      }
    }
  }
  private val withFunctorSymmetryBreaker2 = {
    withFunctor match {
      case None => IndexedSeq.fill(rank)(true)
      case Some((restriction, _)) => {
        val t = restriction.toSeq.map(_.toSeq).transpose
        IndexedSeq.tabulate(rank)({ i =>
          i <= 1 || t(i) == t(i - 2)
        })
      }
    }
  }

  private def inequalities(step: Int, x: Array[Int]): Boolean = {
    // all we worry about is that the diagonal entries are descending (or as descending as we can make them, given that we have the dual pairs together, at the end)

    if (withMatrix.isEmpty) {
      val v = representativeMultiplicities(step)
      if (v(0) > 1 && v(0) == v(1) && v(1) == v(2)) {
        if ((v(0) < selfDualObjects || (v(0) - selfDualObjects) % 2 == 1) && withFunctorSymmetryBreaker1(v(0))) {
          val pstep = lookup(v(0) - 1)(v(0) - 1)(v(0) - 1).right.get
          require(pstep < step)
          //        println((step, pstep, v(0), v(0)-1, lookup(v(0) - 1)(v(0) - 1)(v(0) - 1)))
          x(step) < x(pstep) || {
            x(step) == x(pstep) && {
              val pstep1 = lookup(v(0) - 1)(v(0) - 1)(v(0)).right.get
              val pstep2 = lookup(v(0))(v(0))(v(0) - 1).right.get
              require(pstep1 < step)
              require(pstep2 < step)
              x(pstep2) <= x(pstep1)
            }
          }
        } else if (v(0) > selfDualObjects && (v(0) - selfDualObjects) % 2 == 0 && withFunctorSymmetryBreaker2(v(0))) {
          val pstep = lookup(v(0) - 2)(v(0) - 2)(v(0) - 2).right.get
          require(pstep < step)
          x(step) < x(pstep) || {
            x(step) == x(pstep) && {
              val pstep1 = lookup(v(0) - 2)(v(0) - 2)(v(0)).right.get
              val pstep2 = lookup(v(0))(v(0))(v(0) - 2).right.get
              require(pstep1 < step)
              require(pstep2 < step)
              x(pstep2) <= x(pstep1)
            }
          }
        } else {
          require(v(0) == selfDualObjects || withFunctor.nonEmpty)
          true
        }
      } else {
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

  object AssociativityEquation {
    def apply(a: Int, b: Int, c: Int, d: Int): Option[AssociativityEquation] = {
      var lhsConstant = 0
      var lhsLinear = ListBuffer[Int]()
      var lhsQuadratic = ListBuffer[(Int, Int)]()
      var rhsConstant = 0
      var rhsLinear = ListBuffer[Int]()
      var rhsQuadratic = ListBuffer[(Int, Int)]()

      for (e <- (0 until rank)) {
        (lookup(a)(b)(e), lookup(e)(c)(d)) match {
          case (Left(1), Left(1)) => lhsConstant = lhsConstant + 1
          case (Left(0), _) =>
          case (_, Left(0)) =>
          case (Left(1), Right(i)) => lhsLinear += i
          case (Right(i), Left(1)) => lhsLinear += i
          case (Right(i), Right(j)) if i < j => lhsQuadratic += ((i, j))
          case (Right(i), Right(j)) if i >= j => lhsQuadratic += ((j, i))
        }
        (lookup(a)(e)(d), lookup(b)(c)(e)) match {
          case (Left(1), Left(1)) => rhsConstant = rhsConstant + 1
          case (Left(0), _) =>
          case (_, Left(0)) =>
          case (Left(1), Right(i)) => rhsLinear += i
          case (Right(i), Left(1)) => rhsLinear += i
          case (Right(i), Right(j)) if i < j => rhsQuadratic += ((i, j))
          case (Right(i), Right(j)) if i >= j => rhsQuadratic += ((j, i))
        }
      }

      if (lhsConstant > rhsConstant) {
        lhsConstant = lhsConstant - rhsConstant
        rhsConstant = 0
      } else {
        rhsConstant = rhsConstant - lhsConstant
        lhsConstant = 0
      }

      val swapLinear = lhsLinear -- rhsLinear
      rhsLinear --= lhsLinear
      lhsLinear = swapLinear

      val swapQuadratic = lhsQuadratic -- rhsQuadratic
      rhsQuadratic --= lhsQuadratic
      lhsQuadratic = swapQuadratic

      if (lhsConstant != 0 || rhsConstant != 0 || lhsLinear.nonEmpty || rhsLinear.nonEmpty || lhsQuadratic.nonEmpty || rhsQuadratic.nonEmpty) {
        Some(AssociativityEquation(Quadratic(lhsConstant, lhsLinear.sorted, lhsQuadratic.sorted), Quadratic(rhsConstant, rhsLinear.sorted, rhsQuadratic.sorted)))
      } else {
        None
      }
    }
  }

  case class AssociativityEquation(lhs: Quadratic, rhs: Quadratic) {
    override def equals(other: Any) = {
      other match {
        case AssociativityEquation(olhs, orhs) => (lhs == olhs && rhs == orhs) || (lhs == orhs && rhs == olhs)
        case _ => false
      }
    }
    override def hashCode = {
      val lh = lhs.hashCode
      val rh = rhs.hashCode
      lh + rh
    }

    val variables = lhs.variables ++ rhs.variables
    val lastVariable = {
      import net.tqft.toolkit.arithmetic.MinMax._
      variables.maxOption.getOrElse(-1)
    }
    def declareZero(i: Int): Option[AssociativityEquation] = {
      if (variables.contains(i)) {
        Some(AssociativityEquation(lhs.declareZero(i), rhs.declareZero(i)))
      } else {
        None
      }
    }
    def check(x: Array[Int]): Boolean = {
      lhs.evaluate(x) == rhs.evaluate(x)
    }
    lazy val oneSided_? : Option[Quadratic] = {
      if (lhs.zero_?) {
        Some(rhs)
      } else if (rhs.zero_?) {
        Some(lhs)
      } else {
        None
      }
    }
  }

  case class Quadratic(constant: Int, linear: Seq[Int], quadratic: Seq[(Int, Int)]) {
    val variables = (linear ++ quadratic.map(_._1) ++ quadratic.map(_._2)).toSet
    val zero_? = constant == 0 && variables.isEmpty
    def evaluate(x: Array[Int]): Int = {
      var t = constant
      for (i <- linear) t = t + x(i)
      for ((i, j) <- quadratic) t = t + x(i) * x(j)
      t
    }
    def positive_?(step: Int, x: Array[Int]): Boolean = {
      constant > 0 ||
        linear.exists(i => i <= step && x(i) > 0) ||
        quadratic.exists(p => p._1 <= step && p._2 <= step && x(p._1) * x(p._2) > 0)
    }
    def declareZero(i: Int): Quadratic = {
      if (variables.contains(i)) {
        Quadratic(constant, linear.filterNot(_ == i), quadratic.filterNot(p => p._1 == i || p._2 == i))
      } else {
        this
      }
    }
  }

  object AssociativityData {
    lazy val restrictionEquations = {
      val functor = withFunctor.get._1
      val targetRing = withFunctor.get._2

      println(functor.toList.map(_.toList))
      println(targetRing.toList.map(_.map(_.toList).toList))

      val targetRank = targetRing.length
      for (a <- 1 until rank; b <- a until rank; c <- 0 until targetRank) yield {
        // forget, then multiply
        var constant = {
          (for (d <- 0 until targetRank; e <- 0 until targetRank) yield {
            functor(d)(a) * functor(e)(b) * targetRing(d)(e)(c)
          }).sum
        }
        // multiply, then forget
        val linear = ListBuffer[Int]()
        for (x <- 0 until rank) {
          for (_ <- 0 until functor(c)(x)) {
            lookup(a)(b)(x) match {
              case Right(i) => linear += i
              case Left(i) => constant = constant - i
            }
          }
        }
        //        println((a,b,c,constant, linear.sorted.map(representativeMultiplicities)))
        AssociativityEquation(Quadratic(constant, Nil, Nil), Quadratic(0, linear.sorted, Nil))
      }
    }

    val root: AssociativityData = {
      val associativityEquations = (for (a <- 1 until rank; b <- a until rank; c <- a until rank; d <- a until rank; eq <- AssociativityEquation(a, b, c, d)) yield eq)
      val equations = (associativityEquations ++ (if (withFunctor.nonEmpty) restrictionEquations else Nil)).distinct
      val r = AssociativityData(Nil, equations.groupBy(_.lastVariable), Nil)

      r
    }

    private val cache = {
      val size = 1000000000 / root.toString.size

      CacheBuilder.newBuilder.maximumSize(size).build(new CacheLoader[List[Int], AssociativityData] {
        override def load(z: List[Int]): AssociativityData = {
          z match {
            case Nil => root
            case head :: tail => apply(tail).declareZero(head)
          }
        }
      })
    }

    def apply(z: List[Int]): AssociativityData = cache.get(z)

  }
  case class AssociativityData(zeroes: List[Int], equations: Map[Int, Seq[AssociativityEquation]], obstructions: Seq[Quadratic]) {
    def check(step: Int, x: Array[Int]): Boolean = {
      for (equation <- equations.get(step).getOrElse(Nil)) {
        if (!equation.check(x)) return false
      }
      for (obstruction <- obstructions) {
        if (obstruction.positive_?(step, x)) return false
      }
      true
    }

    def declareZero(i: Int): AssociativityData = {
      //      println("preparing associativity data for " + (i :: zeroes))
      require(zeroes.isEmpty || i > zeroes.head)

      val map = (for (j <- i until numberOfVariables) yield {
        j -> ListBuffer[AssociativityEquation]()
      }).toMap
      val newObstructions = ListBuffer[Quadratic]()
      newObstructions ++= obstructions
      for (j <- i until numberOfVariables; equation <- equations.getOrElse(j, Nil)) {
        equation.declareZero(i) match {
          case Some(newEquation) => {
            if (newEquation.lastVariable <= i) {
              map(i) += newEquation
            } else {
              map(newEquation.lastVariable) += newEquation
            }
            newEquation.oneSided_? match {
              case Some(q) => newObstructions += q
              case None =>
            }
          }
          case None => {
            map(j) += equation
          }
        }
      }

      AssociativityData(i :: zeroes, map.mapValues(_.toList), newObstructions.distinct)
    }
  }
}
  
