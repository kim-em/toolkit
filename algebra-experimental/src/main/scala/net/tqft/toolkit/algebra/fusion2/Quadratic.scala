package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.AdditiveMonoid
import scala.collection.mutable.ListBuffer

trait Substitutable[X <: Substitutable[X, S], S] {
  def substitute(s: S, k: Int): X
  def zero_? : Boolean
  def variables: Set[S]
}

case class SystemOfQuadratics[S](quadratics: Seq[QuadraticHistory[S]]) {
  private lazy val (minimalNumberOfVariables, equationsWithMinimalNumberOfVariables) = {
    var n = quadratics.head.variables.size
    val b = ListBuffer[Quadratic[S]]()
    for (q <- quadratics) {
      if (q.variables.size < n) {
        n = q.variables.size
        b.clear
      }
      if (q.variables.size == n) {
        b += q.current
      }
    }
    (n, b.toList)
  }
  lazy val preferredSubstitutionVariables = {
    val tally = {
      import net.tqft.toolkit.collections.Tally._
      equationsWithMinimalNumberOfVariables.flatMap(_.variables).tally
    }
    val max = tally.map(_._2).max
    tally.filter(_._2 == max).map(_._1)
  }
  def resetHistory = SystemOfQuadratics(quadratics.map(qh => { val q = qh.current.factor; QuadraticHistory(q, q) }))
  def substitute(s: S, k: Int) = {
    val result = SystemOfQuadratics(quadratics.map(_.substitute(s, k)).filter(q => !q.zero_?))
    if (result.minimalNumberOfVariables == 0) {
      None
    } else {
      Some(result)
    }
  }
}

case class QuadraticHistory[S](original: Quadratic[S], current: Quadratic[S]) extends Substitutable[QuadraticHistory[S], S] {
  override def substitute(s: S, k: Int) = QuadraticHistory(original, current.substitute(s, k))
  override def zero_? = current.zero_?
  override def variables = current.variables
}

object QuadraticHistory {
  implicit def promote[S](q: Quadratic[S]): QuadraticHistory[S] = QuadraticHistory(q, q)
}

case class Quadratic[S](linearTerm: LinearTerm[S], quadraticTerms: Seq[QuadraticTerm[S]]) extends Substitutable[Quadratic[S], S] {
  override def toString = {
    if (linearTerm.zero_?) {
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
    }
  }

  def zero_? = linearTerm.zero_? && quadraticTerms.forall(_.zero_?)

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
    if (q.x.constant_?) {
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
      LinearTerm(x.constant + y.constant, (x.terms.keySet ++ y.terms.keySet).map(s => s -> (x.terms.getOrElse(s, 0) + y.terms.getOrElse(s, 0))).filter(_._2 != 0).toMap)
    }
  }
}

case class LinearTerm[S](constant: Int, terms: Map[S, Int]) extends Substitutable[LinearTerm[S], S] {
  def constant_? = terms.isEmpty
  def zero_? = constant == 0 && constant_?
  def multiplyBy(k: Int) = LinearTerm(constant * k, terms.mapValues(_ * k))
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

  override val variables = x.variables ++ y.variables
  override def zero_? = x.zero_? || y.zero_?

  override def substitute(s: S, k: Int): QuadraticTerm[S] = {
    if (variables.contains(s)) {
      QuadraticTerm(a, x.substitute(s, k), y.substitute(s, k))
    } else {
      this
    }
  }
}