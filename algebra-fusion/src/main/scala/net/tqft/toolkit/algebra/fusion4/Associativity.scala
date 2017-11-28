package net.tqft.toolkit.algebra.fusion4

import scala.collection.mutable.ListBuffer

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
  def evaluate(i: (Int, Int, Int), x: Int): Option[AssociativityEquation] = {
    if (x == 0) {
      declareZero(i)
    } else {
      if (variables.contains(i)) {
        val newLhs = lhs.evaluate(i, x)
        val newRhs = rhs.evaluate(i, x)
        // FIXME cancel!!

        Some(AssociativityEquation(newLhs, newRhs))
      } else {
        None
      }
    }
  }
  def declareZero(i: (Int, Int, Int)): Option[AssociativityEquation] = {
    if (variables.contains(i)) {
      Some(AssociativityEquation(lhs.declareZero(i), rhs.declareZero(i)))
    } else {
      None
    }
  }
  //    def check(x: Array[Int]): Boolean = {
  //      lhs.evaluate(x) == rhs.evaluate(x)
  //    }
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

case class Quadratic(constant: Int, linear: Seq[(Int, Int, Int)], quadratic: Seq[((Int, Int, Int), (Int, Int, Int))]) {
  val variables = (linear ++ quadratic.map(_._1) ++ quadratic.map(_._2)).toSet
  val zero_? = constant == 0 && variables.isEmpty
  //    def evaluate(x: Array[Int]): Int = {
  //      var t = constant
  //      for (i <- linear) t = t + x(i)
  //      for ((i, j) <- quadratic) t = t + x(i) * x(j)
  //      t
  //    }
  //    def positive_?(step: Int, x: Array[Int]): Boolean = {
  //      constant > 0 ||
  //        linear.exists(i => i <= step && x(i) > 0) ||
  //        quadratic.exists(p => p._1 <= step && p._2 <= step && x(p._1) * x(p._2) > 0)
  //    }
  def evaluate(i: (Int, Int, Int), x: Int): Quadratic = {
    if (x == 0) {
      declareZero(i)
    } else {
      if (variables.contains(i)) {
        val newConstant = constant + x * linear.count(_ == i) + x * x * quadratic.count(_ == (i, i))
        val newLinear = linear.filterNot(_ == i) ++ quadratic.collect({ case (i0, j0) if i0 == i && j0 != i => j0; case (i0, j0) if j0 == i && i0 != i => j0 }).flatMap(Seq.fill(x)(_))
        val newQuadratic = quadratic.filterNot(p => p._1 == i || p._2 == i)
        Quadratic(constant, linear, quadratic)
      } else {
        this
      }
    }
  }
  def declareZero(i: (Int, Int, Int)): Quadratic = {
    if (variables.contains(i)) {
      Quadratic(constant, linear.filterNot(_ == i), quadratic.filterNot(p => p._1 == i || p._2 == i))
    } else {
      this
    }
  }
}

case class AssociativityEquations(rank: Int) {
  def apply(a: Int, b: Int, c: Int, d: Int): Option[AssociativityEquation] = {

    var lhsQuadratic = ListBuffer[((Int, Int, Int), (Int, Int, Int))]()

    var rhsQuadratic = ListBuffer[((Int, Int, Int), (Int, Int, Int))]()

    for (e <- (0 until rank)) yield {
      lhsQuadratic += (((a, b, e), (e, c, d)))
      rhsQuadratic += (((a, e, d), (b, c, e)))

    }

    val swapQuadratic = lhsQuadratic -- rhsQuadratic
    rhsQuadratic --= lhsQuadratic
    lhsQuadratic = swapQuadratic

    if (lhsQuadratic.nonEmpty || rhsQuadratic.nonEmpty) {
      Some(AssociativityEquation(Quadratic(0, Seq.empty, lhsQuadratic.sorted), Quadratic(0, Seq.empty, rhsQuadratic.sorted)))
    } else {
      None
    }
  }

  val root = {
    val associativityEquations = (for (a <- 1 until rank; b <- a until rank; c <- a until rank; d <- a until rank; eq <- apply(a, b, c, d)) yield eq)
    associativityEquations.distinct
  }

}
case class AssociativityData(zeroes: List[Int], equations: Map[Int, Seq[AssociativityEquation]], obstructions: Seq[Quadratic]) {
  //  def check(step: Int, x: Array[Int]): Boolean = {
  //    for (equation <- equations.get(step).getOrElse(Nil)) {
  //      if (!equation.check(x)) return false
  //    }
  //    for (obstruction <- obstructions) {
  //      if (obstruction.positive_?(step, x)) return false
  //    }
  //    true
  //  }
  //
  //  def declareZero(i: Int): AssociativityData = {
  //    //      println("preparing associativity data for " + (i :: zeroes))
  //    require(zeroes.isEmpty || i > zeroes.head)
  //
  //    val map = (for (j <- i until numberOfVariables) yield {
  //      j -> ListBuffer[AssociativityEquation]()
  //    }).toMap
  //    val newObstructions = ListBuffer[Quadratic]()
  //    newObstructions ++= obstructions
  //    for (j <- i until numberOfVariables; equation <- equations.getOrElse(j, Nil)) {
  //      equation.declareZero(i) match {
  //        case Some(newEquation) => {
  //          if (newEquation.lastVariable <= i) {
  //            map(i) += newEquation
  //          } else {
  //            map(newEquation.lastVariable) += newEquation
  //          }
  //          newEquation.oneSided_? match {
  //            case Some(q) => newObstructions += q
  //            case None =>
  //          }
  //        }
  //        case None => {
  //          map(j) += equation
  //        }
  //      }
  //    }
  //
  //    AssociativityData(i :: zeroes, map.mapValues(_.toList), newObstructions.distinct)
  //  }
}
  
