package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.AdditiveMonoid

trait Substitutable[X <: Substitutable[X, S], S] {
  def substitute(s: S, k: Int): X
}

case class Quadratic[S](linearTerm: LinearTerm[S], quadraticTerms: Seq[QuadraticTerm[S]]) extends Substitutable[Quadratic[S], S] {
  lazy val closedByZeroSubstitutions: Seq[S] = ???
  lazy val closedBySubstitutions: Seq[S] = ???

  def factor: Quadratic[S] = {
    // tally up all the linear terms (both the linear term, and factors of the linear terms)
    // if everything just appears once, return this
    // otherwise, pick the linear term that appears most often, and pull that out
    //   then try factoring again
	???
  }

  override def substitute(s: S, k: Int): Quadratic[S] = {
    // substitute in the linear term and the quadratic terms
    // and then check if any quadratic terms have become linear
    var newLinearTerm = linearTerm.substitute(s, k)
    val quadraticSubstitutions = quadraticTerms.map(_.substitute(s, k))
    val newQuadraticTerms = quadraticSubstitutions.flatMap({
      case LinearTerm(l) => {
        import net.tqft.toolkit.algebra.AlgebraicNotation._
        newLinearTerm = newLinearTerm + l
        None
      }
      case q => Some(q)
    })
    Quadratic(newLinearTerm, newQuadraticTerms)
  }
}

object LinearTerm {
  def unapply[S](q: QuadraticTerm[S]): Option[LinearTerm[S]] = {
    if (q.x.constant_?) {
      Some(q.y.multiplyBy(q.x.constant))
    } else if (q.y.constant_?) {
      Some(q.x.multiplyBy(q.y.constant))
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
  def multiplyBy(k: Int) = LinearTerm(constant * k, terms.mapValues(_ * k))

  override def substitute(s: S, k: Int): LinearTerm[S] = {
    terms.get(s) match {
      case Some(x) => LinearTerm(constant + k * x, terms - s)
      case None => this
    }
  }
}

case class QuadraticTerm[S](x: LinearTerm[S], y: LinearTerm[S]) extends Substitutable[QuadraticTerm[S], S] {
  override def substitute(s: S, k: Int): QuadraticTerm[S] = {
    QuadraticTerm(x.substitute(s, k), y.substitute(s, k))
  }
}