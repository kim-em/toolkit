package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.AdditiveMonoid

trait Substitutable[X <: Substitutable[X, S], S] {
  def substitute(s: S, k: Int): X
  def zero_? : Boolean
  def becomesConstantAtZero_?(s: S): Boolean
  def becomesConstant_?(s: S): Boolean
  def variables: Set[S]
}

case class Quadratic[S](linearTerm: LinearTerm[S], quadraticTerms: Seq[QuadraticTerm[S]]) extends Substitutable[Quadratic[S], S] {
  override def toString = {
    if(linearTerm.zero_?) {
      if(quadraticTerms.isEmpty) {
        "0"
      } else {
        quadraticTerms.mkString(" + ")
      } 
        
    } else {
      if(quadraticTerms.isEmpty) {
        linearTerm.toString
      } else {
        linearTerm.toString + " + " + quadraticTerms.mkString(" + ")
      }
    }
  }

  def zero_? = linearTerm.zero_? && quadraticTerms.forall(_.zero_?)
  
  override def becomesConstantAtZero_?(s: S) = linearTerm.becomesConstantAtZero_?(s) && quadraticTerms.forall(t => t.becomesConstantAtZero_?(s))
  override def becomesConstant_?(s: S) = linearTerm.becomesConstant_?(s) && quadraticTerms.forall(t => t.becomesConstant_?(s))
  lazy val closedByZeroSubstitutions: Set[S] = variables.filter(becomesConstantAtZero_?)
  lazy val closedBySubstitutions: Set[S] = variables.filter(becomesConstant_?)

  def factor: Quadratic[S] = {
    // tally up all the linear terms (both the linear term, and factors of the linear terms)
    // if everything just appears once, return this
    // otherwise, pick the linear term that appears most often, and pull that out
    //   then try factoring again
    val tally = {
      import net.tqft.toolkit.collections.Tally._
      (linearTerm +: quadraticTerms.flatMap(term => Seq(term.x, term.y))).tally
    }
    if (tally.forall(_._2 == 1)) {
      this
    } else {
      val max = tally.map(_._2).max
      val index = tally.indexWhere(_._2 == max)
      ???
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
    if(constant == 0) {
      if(terms.nonEmpty) {
        terms.map(t => t._2.toString + " * " + t._1.toString).mkString("(", " + ", ")")
      } else {
        "0"
      }
    } else {
      if(terms.nonEmpty) {
        "(" + constant + terms.map(t => t._2.toString + " * " + t._1.toString).mkString(" + ") + ")"
      } else {
        constant.toString
      }
    }
  } 

  override def variables = terms.keySet
  override def becomesConstantAtZero_?(s: S) = becomesConstant_?(s)
  override def becomesConstant_?(s: S) = terms.size match {
    case 0 => true
    case 1 => terms.get(s).nonEmpty
    case 2 => false
  }
  def becomesZeroAtZero_?(s: S) = constant == 0 && becomesConstantAtZero_?(s)

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
  override def becomesConstantAtZero_?(s: S) = x.becomesZeroAtZero_?(s) || y.becomesZeroAtZero_?(s) || (x.becomesConstantAtZero_?(s) && y.becomesConstantAtZero_?(s))
  override def becomesConstant_?(s: S) = x.becomesConstant_?(s) && y.becomesConstant_?(s)

  override def substitute(s: S, k: Int): QuadraticTerm[S] = {
    if (variables.contains(s)) {
      QuadraticTerm(a, x.substitute(s, k), y.substitute(s, k))
    } else {
      this
    }
  }
}