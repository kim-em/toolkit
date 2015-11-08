package net.tqft.toolkit.algebra.fusion3

import net.tqft.toolkit.algebra.matrices.FrobeniusPerronEigenvalues
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger
import net.tqft.toolkit.functions.Memo
import scala.collection.mutable.ListBuffer
import java.util.concurrent.ConcurrentHashMap

case class Enumeration(selfDualObjects: Int, dualPairs: Int, globalDimensionBound: Double) {
  val rank = selfDualObjects + 2 * dualPairs
  require(dualPairs == 0)

  private val multiplicities = for (i <- 1 until rank; j <- 1 until rank; k <- 1 until rank) yield Seq(i, j, k)
  private val representativeMultiplicities = {
    multiplicities.filter(m => m == m.sorted)
  }
  private val numberOfVariables = representativeMultiplicities.size
  private val lookupTable = for (m <- multiplicities) yield representativeMultiplicities.indexOf(m.sorted)

  private def lookup(i: Int, j: Int, k: Int): Either[Int, Int] = {
    if (i == 0) {
      if (j == k) Left(1) else Left(0)
    } else {
      if (j == 0) {
        if (i == k) Left(1) else Left(0)
      } else {
        if (k == 0) {
          if (i == j) Left(1) else Left(0)
        } else {
          Right(lookupTable((i - 1) * (rank - 1) * (rank - 1) + (j - 1) * (rank - 1) + (k - 1)))
        }
      }
    }
  }
  private val lookupDiagonals = (for (i <- 1 until rank) yield representativeMultiplicities.indexOf(Seq(i, i, i))).sorted

  def N(x: Array[Int])(i: Int, j: Int, k: Int) = {
    lookup(i, j, k) match {
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
          t = t + N(zeroes)(k, j, l) * N(zeroes)(k, l, i)
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

  case class Partial(step: Int, x: Array[Int], zeroes: List[Int], r: Array[Array[Int]], hint: Array[Double] = Array.fill(rank)(1.0), checkedAssociativities: Set[(Int, Int, Int, Int)] = Set.empty) {
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
      require(m < 25)

      val nextX = x.clone()
      nextX(step + 1) = m

      if (inequalities(step + 1, nextX)) {

        if (m == 0) {
          Some(Partial(step + 1, x, (step + 1) :: zeroes, r, hint))
        } else {

          val nextR = Array.tabulate(rank, rank)({ (i, j) =>
            var t = r(i)(j)
            for ((k, l) <- Rterms(step + 2)(i)(j)) {
              t = t + N(nextX)(k, j, l) * N(nextX)(k, l, i)
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
      if(AssociativityData(zeroes).check(step, x)) {
        Some(this)
      } else {
        None
      }
    }

//    def associativeAt_?(a: Int, b: Int, c: Int, d: Int): Boolean = {
//      var t = 0
//      for (e <- (0 until rank)) {
//        t = t + N(x)(a, b, e) * N(x)(e, c, d) - N(x)(a, e, d) * N(x)(b, c, e)
//      }
//      t == 0
//    }

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

  def inequalities(step: Int, x: Array[Int]): Boolean = {
    val v = representativeMultiplicities(step)
    if (v(0) > 1 && v(0) == v(1) && v(1) == v(2)) {
      x(step) <= x(lookup(v(0) - 1, v(0) - 1, v(0) - 1).right.get)
    } else {
      true
    }
  }

  val Rterms = {
    import net.tqft.toolkit.arithmetic.MinMax._

    val terms = for (i <- 0 until rank; j <- 0 until rank; k <- 0 until rank; l <- 0 until rank) yield {
      (i, j, (k, l), Seq(lookup(k, j, l), lookup(k, l, i)).collect({ case Right(z) => z }).maxOption.getOrElse(-1))
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

//  def associativitiesByVariableWithZeroes(zeroes: Set[Int]) = {
//    println("computing associativites for " + zeroes)
//    val equations = (for (x <- 1 until rank; y <- x until rank; z <- x until rank; b <- x until rank) yield {
//      import net.tqft.toolkit.arithmetic.MinMax._
//
//      val p = (for (a <- 1 until rank) yield {
//        val i0 = lookup(x, y, a).right.get
//        val i1 = lookup(a, z, b).right.get
//        val i2 = lookup(x, a, b).right.get
//        val i3 = lookup(y, z, a).right.get
//
//        (if (zeroes.contains(i0) || zeroes.contains(i1)) {
//          Seq.empty
//        } else {
//          Seq(i0, i1)
//        }) ++ (if (zeroes.contains(i2) || zeroes.contains(i3)) {
//          Seq.empty
//        } else {
//          Seq(i2, i3)
//        })
//      }).flatten.maxOption.getOrElse(-1)
//      ((x, y, z, b), p)
//    })
//    val map = equations.groupBy(_._2).mapValues(_.map(_._1)).withDefaultValue(IndexedSeq.empty)
//    map
//  }
//
//  val associativitiesByVariable = {
//    //    val n = Seq(5, numberOfVariables).min
//    //    import net.tqft.toolkit.collections.Subsets._
//    //    import net.tqft.toolkit.collections.KSubsets._
//    //    val sets = ((0 until n).subsets.map(_.toSet) ++ (0 until numberOfVariables).kSubsets(3).map(_.toSet)).toSet
//    Memo(associativitiesByVariableWithZeroes _)
//  }

  object AssociativityEquation {
    def apply(a: Int, b: Int, c: Int, d: Int): Option[AssociativityEquation] = {
      var lhsConstant = 0
      var lhsLinear = ListBuffer[Int]()
      var lhsQuadratic = ListBuffer[(Int, Int)]()
      var rhsConstant = 0
      var rhsLinear = ListBuffer[Int]()
      var rhsQuadratic = ListBuffer[(Int, Int)]()

      for (e <- (0 until rank)) {
        (lookup(a, b, e), lookup(e, c, d)) match {
          case (Left(1), Left(1)) => lhsConstant = lhsConstant + 1
          case (Left(0), _) =>
          case (_, Left(0)) =>
          case (Left(1), Right(i)) => lhsLinear += i
          case (Right(i), Left(1)) => lhsLinear += i
          case (Right(i), Right(j)) if i < j => lhsQuadratic += ((i, j))
          case (Right(i), Right(j)) if i >= j => lhsQuadratic += ((j, i))
        }
        (lookup(a, e, d), lookup(b, c, e)) match {
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
        Some(AssociativityEquation((a, b, c, d), Quadratic(lhsConstant, lhsLinear.sorted, lhsQuadratic.sorted), Quadratic(rhsConstant, rhsLinear.sorted, rhsQuadratic.sorted)))
      } else {
        None
      }
    }
  }

  case class AssociativityEquation(
      tuple: (Int, Int, Int, Int),
      lhs: Quadratic, rhs: Quadratic) {
    val variables = lhs.variables ++ rhs.variables
    val lastVariable = {
      import net.tqft.toolkit.arithmetic.MinMax._
      variables.maxOption.getOrElse(-1)
    }
    def declareZero(i: Int): Option[AssociativityEquation] = {
      if (variables.contains(i)) {
        Some(AssociativityEquation(tuple, lhs.declareZero(i), rhs.declareZero(i)))
      } else {
        None
      }
    }
    def check(x: Array[Int]): Boolean = {
      lhs.evaluate(x) == rhs.evaluate(x)
    }
    def oneSided_? : Option[Quadratic] = {
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
    def zero_? = constant == 0 && variables.isEmpty
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
      val equations = (for (a <- 1 until rank; b <- a until rank; c <- a until rank; d <- a until rank; eq <- AssociativityEquation(a, b, c, d)) yield eq).toSet
      AssociativityData(Nil, equations.groupBy(_.lastVariable), Set.empty)
    }

    private val cache = {
      import scala.collection.JavaConverters._
      new ConcurrentHashMap[List[Int], AssociativityData]().asScala
    }
    cache(Nil) = root

    def apply(z: List[Int]): AssociativityData = {
      val z0 = z
      cache.getOrElseUpdate(z0, apply(z0.tail).declareZero(z0.head))
    }
  }
  case class AssociativityData(zeroes: List[Int], equations: Map[Int, Set[AssociativityEquation]], obstructions: Set[Quadratic]) {
    def check(step: Int, x: Array[Int]): Boolean = {
      for (equation <- equations.get(step).getOrElse(Set.empty)) {
        if(!equation.check(x)) return false
      }
      for(obstruction <- obstructions) {
        if(obstruction.positive_?(step, x)) return false
      }
      true
    }

    def declareZero(i: Int): AssociativityData = {
      //      println("preparing associativity data for " + (i :: zeroes))
      require(zeroes.isEmpty || i > zeroes.head)

      val map = (for (j <- i until numberOfVariables) yield {
        j -> scala.collection.mutable.Set[AssociativityEquation]()
      }).toMap
      val newObstructions = scala.collection.mutable.Set[Quadratic]() ++ obstructions
      for (j <- i until numberOfVariables; equation <- equations.getOrElse(j, Set.empty)) {
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

      AssociativityData(i :: zeroes, map.mapValues(_.toSet), newObstructions.toSet)
    }
  }
}
  
