package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.AdditiveMonoid
import scala.collection.mutable.ListBuffer
import net.tqft.toolkit.Logging

trait Substitutable[X <: Substitutable[X, S], S] {
  def substitute(s: S, k: Int): X
  def zero_? : Boolean
  def constant_? : Boolean
  def variables: Set[S]
}

case class SystemOfQuadratics[S](closedVariables: Set[S], quadratics: Seq[QuadraticState[S]]) {
  lazy val closedVariablesByNumberOfVariables: Map[S, Map[Int, Int]] = {
    import net.tqft.toolkit.collections.Tally._
    (for (s <- closedVariables) yield {
      s -> quadratics.flatMap(q => q.closedVariablesByNumberOfVariables.get(s)).tally
    }).toMap
  }
  lazy val quadraticsWithFewestVariables: Seq[Quadratic[S]] = {
    val nonzeroQuadratics= quadratics.filterNot(_.zero_?)
    if (nonzeroQuadratics.isEmpty) {
      Nil
    } else {
      var min = nonzeroQuadratics.head.variables.size
      val result = ListBuffer[Quadratic[S]]()
      for (q <- nonzeroQuadratics.map(_.completeSubstitution)) {
        if (q.variables.size < min) {
          result.clear
          min = q.variables.size
        }
        if (q.variables.size == min) {
          result += q
        }
      }
      result
    }
  }
//  def mostFrequestVariablesInQuadraticsWithFewestVariables(amongst: Set[S]): Set[S] = {
//    if (quadratics.isEmpty) {
//      amongst
//    } else {
//      Logging.info(s"amongst: $amongst")
//      import net.tqft.toolkit.collections.Tally._
//      val tally = quadraticsWithFewestVariables.flatMap(_.variables.intersect(amongst)).tally
//      if (tally.isEmpty) {
//        amongst
//      } else {
//        val max = tally.map(_._2).max
//        tally.collect({ case (s, f) if f == max => s }).toSet
//      }
//    }
//  }
  def variables = quadratics.flatMap({ q: QuadraticState[S] => q.variables }).toSet
  // If substitution is slow, we could easily remove many more duplicates, by sorting terms, or multiplying through by -1

  def substitute(s: S, k: Int, levelOverride: Option[Int] = None): Option[SystemOfQuadratics[S]] = {
    val newQuadratics = quadratics.map(_.substitute(s, k))
    if (newQuadratics.exists(_.completeSubstitution.impossibleAtLevel(levelOverride.getOrElse(k)))) {
      None
    } else {
      Some(SystemOfQuadratics(closedVariables + s, newQuadratics))
    }
  }
  def factor = SystemOfQuadratics(Set.empty, quadratics.map(_.factor).filter(q => !q.zero_?).distinct)

  def mapVariables[T](f: S => T) = SystemOfQuadratics(closedVariables.map(f), quadratics.map(_.mapVariables(f)))

  override def toString = {
    quadratics.mkString("{\n  ", ",\n  ", "\n}")
  }
}

case class QuadraticState[S](label: (Int, Int, Int, Int), completeSubstitution: Quadratic[S], partialSubstitutions: Map[S, Quadratic[S]]) extends Substitutable[QuadraticState[S], S] {
  override def zero_? = completeSubstitution.zero_?
  override def constant_? = completeSubstitution.constant_?
  override def variables = completeSubstitution.variables
  lazy val closedVariablesByNumberOfVariables = partialSubstitutions.mapValues(_.variables.size)
  def closedVariables = partialSubstitutions.keys

  def mapVariables[T](f: S => T) = QuadraticState(label, completeSubstitution.mapVariables(f), partialSubstitutions.map(p => (f(p._1), p._2.mapVariables(f))))

  override def toString = {
    "QuadraticState(" + label + ",\n    " + completeSubstitution + ",\n" + partialSubstitutions.mkString("    Map(", "\n        ", "))") + "     closedVariables =  " + closedVariablesByNumberOfVariables
  }

  override def equals(other: Any) = {
    other match {
      case other: QuadraticState[S] => completeSubstitution == other.completeSubstitution && partialSubstitutions == other.partialSubstitutions
      case _ => false
    }
  }
  override def hashCode = (completeSubstitution, partialSubstitutions).hashCode

  override def substitute(s: S, k: Int) = {
    QuadraticState(
      label,
      completeSubstitution.substitute(s, k),
      (partialSubstitutions.mapValues(q => q.substitute(s, k)) + (s -> completeSubstitution)).filter(m => m._2.variables.contains(m._1)))
  }
  def factor: QuadraticState[S] = {
    QuadraticState(label, completeSubstitution.factor, Map.empty)
  }
}

object QuadraticState {
  def apply[S](label: (Int, Int, Int, Int), quadratic: Quadratic[S]): QuadraticState[S] = QuadraticState(label, quadratic, Map.empty)
}

case class Quadratic[S](linearTerm: LinearTerm[S], quadraticTerms: Seq[QuadraticTerm[S]]) extends Substitutable[Quadratic[S], S] {
  override def toString = {
    (if (linearTerm.zero_?) {
      if (quadraticTerms.isEmpty) {
        "0"
      } else {
        quadraticTerms.mkString(" + ")
      }
    } else {
      if (quadraticTerms.isEmpty) {
        linearTerm.toString
      } else {
        linearTerm.toString + " + " + quadraticTerms.mkString(" + ")
      }
    })
  }

  def mapVariables[T](f: S => T) = Quadratic(linearTerm.mapVariables(f), quadraticTerms.map(_.mapVariables(f)))

  def impossibleAtLevel(k: Int) = {
    val result = constant_? && !zero_? || {
      import net.tqft.toolkit.algebra.Rationals
      quadraticTerms.isEmpty && linearTerm.terms.size == 1 && Rationals.lt(Rationals.subtract(Rationals.quotient(-linearTerm.constant, linearTerm.terms.head._2), k), 0)
    } || {
      linearTerm.constant_? && quadraticTerms.size == 1 && quadraticTerms.head.x.constant == 0 && quadraticTerms.head.y.constant == 0 && quadraticTerms.head.x.terms.size == 1 && quadraticTerms.head.y.terms.size == 1 &&
        {
          import net.tqft.toolkit.algebra.Rationals
          Rationals.lt(Rationals.subtract(Rationals.quotient(-linearTerm.constant, quadraticTerms.head.a * quadraticTerms.head.x.terms.head._2 * quadraticTerms.head.y.terms.head._2), k * k), 0)
        }
    }
    //    if (result) println(s"Claiming $this is impossible at level $k")
    result
  }

  def constant_? = quadraticTerms.forall(_.zero_?) && linearTerm.constant_?
  def zero_? = linearTerm.zero_? && quadraticTerms.forall(_.zero_?)
  def sign = {
    val signs = if (linearTerm.zero_?) {
      quadraticTerms.map(_.sign)
    } else {
      linearTerm.sign +: quadraticTerms.map(_.sign)
    }
    if (signs.forall(_ == 1)) {
      1
    } else if (signs.forall(_ == -1)) {
      -1
    } else {
      0
    }
  }

  def factor: Quadratic[S] = {
    // tally up all the linear terms (both the linear term, and factors of the linear terms)
    // if everything just appears once, return this
    // otherwise, pick the linear term that appears most often, and pull that out
    //   then try factoring again
    val tally = {
      import net.tqft.toolkit.collections.Tally._
      (linearTerm +: quadraticTerms.flatMap(term => Set(term.x, term.y))).tally
    }
    if (tally.forall(_._2 == 1)) {
      this
    } else {
      import net.tqft.toolkit.algebra.AlgebraicNotation._
      val max = tally.map(_._2).max
      val term = tally.find(_._2 == max).get._1
      val newLinearTerm: LinearTerm[S] = if (linearTerm == term) {
        LinearTerm(0, Map.empty)
      } else {
        linearTerm
      }
      val newQuadraticTerms = quadraticTerms.filter(t => t.x != term && t.y != term)
      val coefficient = {
        val linearCoefficient = {
          if (linearTerm == term) {
            LinearTerm[S](1, Map.empty)
          } else {
            LinearTerm[S](0, Map.empty)
          }
        }

        val quadraticCoefficient = {
          val monoid = implicitly[AdditiveMonoid[LinearTerm[S]]]
          monoid.sum(quadraticTerms.flatMap({ t =>
            if (t.x == term) {
              Some(t.y.multiplyBy(t.a))
            } else if (t.y == term) {
              Some(t.x.multiplyBy(t.a))
            } else {
              None
            }
          }))
        }
        linearCoefficient + quadraticCoefficient
      }
      (QuadraticTerm(1, coefficient, term) match {
        case LinearTerm(l) => Quadratic(newLinearTerm + l, newQuadraticTerms.sortBy(_.hashCode))
        case e => Quadratic(newLinearTerm, (e +: newQuadraticTerms).sortBy(_.hashCode))
      }).factor
    }
  }
  override val variables = linearTerm.variables ++ quadraticTerms.flatMap(term => term.variables)

  private val substitutionsCache = net.tqft.toolkit.functions.Memo.softly(substituteImpl _)
  override def substitute(s: S, k: Int): Quadratic[S] = {
    substitutionsCache(s, k)
    //    substituteImpl(s, k)
  }
  def substituteImpl(s: S, k: Int): Quadratic[S] = {
    // substitute in the linear term and the quadratic terms
    // and then check if any quadratic terms have become linear
    if (variables.contains(s)) {
      var newLinearTerm = linearTerm.substitute(s, k)
      val newQuadraticTerms = quadraticTerms.flatMap({ term =>
        term.substitute(s, k) match {
          case LinearTerm(l) => {
            import net.tqft.toolkit.algebra.AlgebraicNotation._
            newLinearTerm = newLinearTerm + l
            None
          }
          case q => Some(q)
        }
      })
      Quadratic(newLinearTerm, newQuadraticTerms)
    } else {
      this
    }
  }
}

object LinearTerm {
  def unapply[S](q: QuadraticTerm[S]): Option[LinearTerm[S]] = {
    val monoid = implicitly[AdditiveMonoid[LinearTerm[S]]]
    if (q.a == 0 || q.x.zero_? || q.y.zero_?) {
      Some(monoid.zero)
    } else if (q.x.constant_?) {
      Some(q.y.multiplyBy(q.x.constant).multiplyBy(q.a))
    } else if (q.y.constant_?) {
      Some(q.x.multiplyBy(q.y.constant).multiplyBy(q.a))
    } else {
      None
    }
  }

  implicit def MonoidStructure[S]: AdditiveMonoid[LinearTerm[S]] = new AdditiveMonoid[LinearTerm[S]] {
    override def zero = LinearTerm(0, Map.empty)
    override def add(x: LinearTerm[S], y: LinearTerm[S]) = {
      LinearTerm(
        x.constant + y.constant,
        (x.terms.keySet ++ y.terms.keySet).map(s => s -> (x.terms.getOrElse(s, 0) + y.terms.getOrElse(s, 0))).filter(_._2 != 0).toMap)
    }
  }
}

case class LinearTerm[S](constant: Int, terms: Map[S, Int]) extends Substitutable[LinearTerm[S], S] {
  def constant_? = terms.isEmpty
  def zero_? = (constant == 0) && constant_?
  def sign = {
    if (constant >= 0 && terms.values.forall(_ > 0)) {
      1
    } else if (constant <= 0 && terms.values.forall(_ < 0)) {
      -1
    } else {
      0
    }
  }
  def multiplyBy(k: Int) = {
    require(k != 0)
    LinearTerm(constant * k, terms.mapValues(_ * k))
  }
  def mapVariables[T](f: S => T) = LinearTerm(constant, terms.map(p => (f(p._1), p._2)))
  override def toString = {
    if (constant == 0) {
      if (terms.nonEmpty) {
        terms.map(t => t._2.toString + " * " + t._1.toString).mkString("(", " + ", ")")
      } else {
        "0"
      }
    } else {
      if (terms.nonEmpty) {
        "(" + constant + " + " + terms.map(t => t._2.toString + " * " + t._1.toString).mkString(" + ") + ")"
      } else {
        constant.toString
      }
    }
  }
  override def variables = terms.keySet

  override def substitute(s: S, k: Int): LinearTerm[S] = {
    terms.get(s) match {
      case Some(x) => LinearTerm(constant + k * x, terms - s)
      case None => this
    }
  }
}

case class QuadraticTerm[S](a: Int, x: LinearTerm[S], y: LinearTerm[S]) extends Substitutable[QuadraticTerm[S], S] {
  override def toString = (if (a == 1) "" else { a.toString + " * " }) + x.toString ++ y.toString

  def mapVariables[T](f: S => T) = QuadraticTerm(a, x.mapVariables(f), y.mapVariables(f))

  override val variables = x.variables ++ y.variables
  override def zero_? = x.zero_? || y.zero_?
  override def constant_? = x.zero_? || y.zero_? || x.constant_? && y.constant_?
  def sign = a.signum * x.sign * y.sign

  override def substitute(s: S, k: Int): QuadraticTerm[S] = {
    if (variables.contains(s)) {
      QuadraticTerm(a, x.substitute(s, k), y.substitute(s, k))
    } else {
      this
    }
  }
}