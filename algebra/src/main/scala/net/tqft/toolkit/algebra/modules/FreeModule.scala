package net.tqft.toolkit.algebra.modules

import net.tqft.toolkit.algebra._
import scala.collection.GenIterable

object FreeModule {
  implicit def orderingToLinearComboOrdering[A, B](implicit o: Ordering[B]) = new Ordering[Map[B, A]] {
    def compare(x: Map[B, A], y: Map[B, A]) = {
      o.compare(x.map(_._1).max, y.map(_._1).max)
    }
  }
}

trait LinearCombo[A, B] {
  def terms: Seq[(B, A)]

  def coefficientOf(b: B): Option[A] = terms.find(_._1 == b).map(_._2)

  override def toString = {
    terms match {
      case Nil => "0"
      case _ => terms.map({ case (g, p) => p.toString + " * " + g.toString }).mkString(" + ")
    }
  }
  override def equals(other: Any) = {
    other match {
      case other: LinearCombo[_, _] => terms.toSet == other.terms.toSet
      case _ => false
    }
  }

  override def hashCode: Int = terms.toSet[(B, A)].hashCode
}

trait GeneralFreeModule[A, B, LC <: LinearCombo[A, B]] extends Module[A, LC] {
  import AlgebraicNotation._

  implicit def ring: Ring[A]

  def wrap(x: Seq[(B, A)]): LC
  def wrap(x: Map[B, A]): LC = wrap(x.toSeq)

  protected def discardZeros(x: Seq[(B, A)]) = (x filterNot (_._2 == ring.zero))
  protected def collect(x: Iterable[(B, A)]): Seq[(B, Iterable[A])] = (x.groupBy(_._1).seq.mapValues({ v => v map (_._2) })).toSeq
  protected def reduce(x: Iterable[(B, A)]) = discardZeros(collect(x) map { case (b, t) => (b, ring.add(t.toSeq)) })
  def simplify(x: Iterable[(B, A)]): LC = wrap(reduce(x))

  override def negate(x: LC) = {
    simplify(x.terms map { case (b, a) => (b, -a) })
  }
  override def scalarMultiply(a: A, x: LC) = {
    simplify(x.terms map (t => (t._1, a * t._2)))
  }
  override def add(x: LC, y: LC) = {
    simplify(x.terms ++ y.terms)
  }
  override def add(xs: GenIterable[LC]) = simplify(xs.flatMap(_.terms).seq)  

  def zero = wrap(Nil)
}

trait FreeModule[A, B] extends GeneralFreeModule[A, B, LinearCombo[A, B]]

trait MapFreeModule[A, B] extends FreeModule[A, B] {
  def wrap(x: List[(B, A)]) = {
    require(x.map(_._1).distinct.size == x.size)
    new LinearCombo[A, B] {
      val terms = x
    }
  }
}

trait IntegerFreeModule[B, LC <: LinearCombo[Int, B]] extends GeneralFreeModule[Int, B, LC] {
  val ring = Gadgets.Integers
}

trait FreeModuleOnMonoid[A, B, LC <: LinearCombo[A, B]] extends GeneralFreeModule[A, B, LC] with Ring[LC] {
  def monoid: CommutativeMonoid[B]

  def multiply(x: LC, y: LC) = {
    simplify(for ((bx, ax) <- x.terms; (by, ay) <- y.terms) yield (monoid.add(bx, by) -> ring.multiply(ax, ay)))
  }
  def one = wrap(Map(monoid.zero -> ring.one))

  def monomial(b: B): LC = wrap(Map(b -> ring.one))
  def monomial(b: B, a: A): LC = wrap(Map(b -> a))

  def constant(a: A): LC = monomial(monoid.zero, a)
  override def fromInt(x: Int) = constant(ring.fromInt(x))
}

trait FancyFreeModule[A, B, LC <: LinearCombo[A, B], K <: Ordered[K]] extends GeneralFreeModule[A, B, LC] {
  def invariant: B => K
  def equivalence: (B, B) => Boolean

  import net.tqft.toolkit.collections.GroupBy._
  override def collect(x: Iterable[(B, A)]) = (x.equivalenceClasses((p1, p2) => equivalence(p1._1, p2._1), p => invariant(p._1)) map { l => (l.head._1 -> l.map(p => p._2)) })
}