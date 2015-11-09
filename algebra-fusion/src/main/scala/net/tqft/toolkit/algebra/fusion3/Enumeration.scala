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

case class Enumeration(selfDualObjects: Int, dualPairs: Int, globalDimensionBound: Double) {
  val rank = selfDualObjects + 2 * dualPairs

  private val dualData = ((0 until selfDualObjects) ++ (for (i <- 0 until dualPairs; n <- Seq(selfDualObjects + 2 * i + 1, selfDualObjects + 2 * i)) yield n)).toIndexedSeq

  private val multiplicities = for (i <- 1 until rank; j <- 1 until rank; k <- 1 until rank) yield Seq(i, j, k)

    def minReciprocal(v: Seq[Int]) = {
    import Ordering.Implicits._
      v match {
        case Seq(i, j, k) => Seq(Seq(i, j, k), Seq(dualData(i), k, j), Seq(j, dualData(k), dualData(i)), Seq(dualData(j), dualData(i), dualData(k)), Seq(dualData(k), i, dualData(j)), Seq(k, dualData(j), i)).min
      }
    }
  private val representativeMultiplicities = {
    multiplicities.filter(m => m == minReciprocal(m))
  }
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

  private val lookupDiagonals = (for (i <- 1 until rank) yield representativeMultiplicities.indexOf(Seq(i, i, i))).sorted

  def N(x: Array[Int])(i: Int, j: Int, k: Int) = {
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
    Partial(-1, zeroes, Nil, r)
  }

  case class Complete(m: Seq[Seq[Seq[Int]]], r: Seq[Seq[Int]], globalDimensionEstimate: Double) {
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
      Complete(
        IndexedSeq.tabulate(rank, rank, rank)({ (i, j, k) => m(p(i))(p(j))(p(k)) }),
        IndexedSeq.tabulate(rank, rank)({ (i, j) => r(p(i))(p(j)) }),
        globalDimensionEstimate)
    }

    lazy val canonicalize: Complete = {
      import Ordering.Implicits._
      implicit val ordering = Ordering.by({ c: Complete => c.m })

      (for (p <- Permutations.of((1 to (rank - 1)).toList); q = (0 +: p).toIndexedSeq) yield permute(q)).min
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

  case class Partial(step: Int, x: Array[Int], zeroes: List[Int], r: Array[Array[Int]], hint: Array[Double] = Array.fill(rank)(1.0)) {
    override def toString = {
      val sep = if (x.max > 9) "," else ""
      x.take(step + 1).mkString(sep)
    }

    def complete: Option[Complete] = {
      if (done_?) {
        val m = Seq.tabulate(rank, rank, rank)({ (i, j, k) => N(x)(i, j, k) })
        Some(Complete(m, r.map(_.toSeq).toSeq, FrobeniusPerronEigenvalues.estimate(r)))
      } else {
        None
      }
    }

    def next(m: Int): Option[Partial] = {
      require(m < 100)
      val nextX = x.clone()
      nextX(step + 1) = m

      if (inequalities(step + 1, nextX)) {

        if (m == 0) {
          Some(Partial(step + 1, x, (step + 1) :: zeroes, r, hint))
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
            Some(Partial(step + 1, nextX, zeroes, nextR, nextHint))
          } else {
            None
          }
        }
      } else {
        None
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
      Iterator.from(0).map(next).takeWhile(_.nonEmpty).map(_.get).flatMap(_.associative_?).toSeq
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
    // all we worry about is that the diagonal entries are descending (or as descending as we can make them, given that we have the dual pairs together, at the end)

    val v = representativeMultiplicities(step)
    if (v(0) > 1 && v(0) == v(1) && v(1) == v(2)) {
      if (v(0) < selfDualObjects || (v(0) - selfDualObjects) % 2 == 1) {
        x(step) <= x(lookup(v(0) - 1)(v(0) - 1)(v(0) - 1).right.get)
      } else if (v(0) > selfDualObjects && (v(0) - selfDualObjects) % 2 == 0) {
        x(step) <= x(lookup(v(0) - 2)(v(0) - 2)(v(0) - 2).right.get)
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
    val root: AssociativityData = {
      val equations = (for (a <- 1 until rank; b <- a until rank; c <- a until rank; d <- a until rank; eq <- AssociativityEquation(a, b, c, d)) yield eq).distinct
      AssociativityData(Nil, equations.groupBy(_.lastVariable), Nil)
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
  
