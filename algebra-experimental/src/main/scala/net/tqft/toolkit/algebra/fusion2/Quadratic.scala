package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.AdditiveMonoid
import scala.collection.mutable.ListBuffer

trait Substitutable[X <: Substitutable[X, S], S] {
  def substitute(s: S, k: Int): X
  def zero_? : Boolean
  def constant_? : Boolean
  def variables: Set[S]
}

// TODO split up quadratics which are sums of terms of the same sign.

case class SystemOfQuadratics[S](closedVariables: Set[S], quadratics: Seq[QuadraticState[S]]) {
  lazy val variableTallies = {
    import net.tqft.toolkit.collections.Tally._
    quadratics.flatMap(_.variables).tally
  }
  lazy val mostFrequentVariables = {
    if (variableTallies.isEmpty) {
      Seq.empty
    } else {
      val max = variableTallies.map(_._2).max
      variableTallies.filter(_._2 == max).map(_._1)
    }
  }
  lazy val closedVariableTallies = {
    import net.tqft.toolkit.collections.Tally._
    quadratics.flatMap(_.closedVariables).tally.toMap
  }
  lazy val closedVariablesByNumberOfVariables: Map[S, Map[Int, Int]] = {
    import net.tqft.toolkit.collections.Tally._
    (for (s <- closedVariables) yield {
      s -> quadratics.map(q => q.closedVariablesByNumberOfVariables.get(s).getOrElse(q.completeSubstitution.variables.size)).tally.toMap
    }).toMap
  }
  def variables = quadratics.flatMap({ q: QuadraticState[S] => q.variables }).toSet
  // If substitution is slow, we could easily remove many more duplicates, by sorting terms, or multiplying through by -1
  def substitute(s: S, k: Int): Option[SystemOfQuadratics[S]] = {
    val newQuadratics = quadratics.map(_.substitute(s, k))
    if (newQuadratics.exists({ q: QuadraticState[S] => q.constant_? && !q.zero_? })) {
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

case class QuadraticState[S](completeSubstitution: Quadratic[S], partialSubstitutions: Map[S, Quadratic[S]]) extends Substitutable[QuadraticState[S], S] {
  override def zero_? = completeSubstitution.zero_?
  override def constant_? = completeSubstitution.constant_?
  override def variables = completeSubstitution.variables
  lazy val closedVariablesByNumberOfVariables = partialSubstitutions.mapValues(_.variables.size)
  def closedVariables = partialSubstitutions.keys

  def mapVariables[T](f: S => T) = QuadraticState(completeSubstitution.mapVariables(f), partialSubstitutions.map(p => (f(p._1), p._2.mapVariables(f))))

  override def toString = {
    "QuadraticState(" + completeSubstitution + ",\n" + partialSubstitutions.mkString("    Map(", "\n        ", "))") + "     closedVariables =  " + closedVariablesByNumberOfVariables
  }

  override def substitute(s: S, k: Int) = {
    if (variables.contains(s)) {
      QuadraticState(
        completeSubstitution.substitute(s, k),
        partialSubstitutions.mapValues(q => q.substitute(s, k)).filter(m => m._2.variables.contains(m._1)) + (s -> completeSubstitution))
    } else {
      this
    }
  }
  def factor: QuadraticState[S] = {
    QuadraticState(completeSubstitution.factor, Map.empty)
  }
}

object QuadraticState {
  def apply[S](quadratic: Quadratic[S]): QuadraticState[S] = QuadraticState(quadratic, Map.empty)
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

  def constant_? = quadraticTerms.forall(_.zero_?) && linearTerm.constant_?
  def zero_? = linearTerm.zero_? && quadraticTerms.forall(_.zero_?)
  def sign = {
    val signs = if(linearTerm.zero_?) {
     quadraticTerms.map(_.sign)
    } else {
      linearTerm.sign +: quadraticTerms.map(_.sign)
    }
    if(signs.forall(_ == 1)) {
      1
    } else if(signs.forall(_ == -1)) {
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
        case LinearTerm(l) => Quadratic(newLinearTerm + l, newQuadraticTerms)
        case e => Quadratic(newLinearTerm, e +: newQuadraticTerms)
      }).factor
    }
  }
  override val variables = linearTerm.variables ++ quadraticTerms.flatMap(term => term.variables)

  override def substitute(s: S, k: Int): Quadratic[S] = {
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
        "(" + constant + terms.map(t => t._2.toString + " * " + t._1.toString).mkString(" + ") + ")"
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