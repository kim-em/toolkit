package net.tqft.toolkit.algebra.modules

import net.tqft.toolkit.algebra._
import scala.collection.GenTraversableOnce

object FreeModule {
  // FIXME this isn't much of an ordering...
  //  implicit def orderingToLinearComboOrdering[A, B](implicit o: Ordering[B]) = new Ordering[Map[B, A]] {
  //    def compare(x: Map[B, A], y: Map[B, A]) = {
  //      o.compare(x.map(_._1).max, y.map(_._1).max)
  //    }
  //  }
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

trait GeneralFreeModuleOverRig[A, B, LC <: LinearCombo[A, B]] extends ModuleOverRig[A, LC] {
  implicit def ring: Rig[A]

  def wrap(x: Seq[(B, A)]): LC
  def wrap(x: Map[B, A]): LC = wrap(x.toSeq)

  protected def discardZeros(x: Seq[(B, A)]) = (x filterNot (_._2 == ring.zero))
  protected def collect(x: Iterable[(B, A)]): Seq[(B, Iterable[A])] = (x.groupBy(_._1).seq.mapValues({ v => v map (_._2) })).toSeq
  protected def reduce(x: Iterable[(B, A)]) = discardZeros(collect(x) map { case (b, t) => (b, ring.sum(t.toSeq)) })
  def simplify(x: Iterable[(B, A)]): LC = wrap(reduce(x))

  override def scalarMultiply(a: A, x: LC) = {
    import AlgebraicNotation._
    if (a == ring.one) {
      x
    } else if (a == ring.zero) {
      zero
    } else {
      simplify(x.terms map (t => (t._1, a * t._2)))
    }
  }
  override def add(x: LC, y: LC) = {
    simplify(x.terms ++ y.terms)
  }
  override def sum(xs: GenTraversableOnce[LC]) = simplify(xs.toSeq.flatMap(_.terms).seq)

  def zero = wrap(Nil)
}

trait MapLinearCombo[A, B] extends (B => A) {
  def toMap: Map[B, A]
  def toSeq = toMap.toSeq
  def apply(b: B) = toMap(b)
}

trait MapFreeModuleOverRig[A, B, M <: MapLinearCombo[A, B]] extends ModuleOverRig[A, M] {
  implicit def ring: Rig[A]
  def wrap(m: Map[B, A]): M
  def unsafeWrap(m: Map[B, A]): M

  override def scalarMultiply(a: A, m: M): M = {
    if (a == ring.one) {
      m
    } else if (a == ring.zero) {
      zero
    } else {
      wrap(m.toMap.mapValues(x => ring.multiply(a, x)))
    }
  }

  override def add(x: M, y: M): M = {
    val xk = x.toMap.keySet
    if (xk.isEmpty) {
      y
    } else {
      val yk = y.toMap.keySet
      if (yk.isEmpty) {
        x
      } else {
        val i = xk.intersect(yk)
        if (i.isEmpty) {
          wrap(x.toMap ++ y.toMap)
        } else {
          wrap((x.toMap -- i) ++ (y.toMap -- i) ++ i.map(k => k -> ring.add(x(k), y(k))))
        }
      }
    }
  }

  //  override def sum(xs: GenTraversableOnce[M]) = {
  //    ???
  //  }

  override def zero = unsafeWrap(Map.empty)
}

trait MapFreeModule[A, B, M <: MapLinearCombo[A, B]] extends MapFreeModuleOverRig[A, B, M] with Module[A, M] {
  override implicit def ring: Ring[A]

  override def negate(x: M) = {
    unsafeWrap(x.toMap.mapValues(ring.negate))
  }
}

trait MapFreeModuleOnMonoidOverRig[A, B, M <: MapLinearCombo[A, B]] extends MapFreeModuleOverRig[A, B, M] with Rig[M] {
  def monoid: AdditiveMonoid[B]

  def multiply(x: M, y: M) = {
    if (x.toMap.size == 1) {
      val (bx, ax) = x.toMap.head
      if (y.toMap.size == 1) {
        val (by, ay) = y.toMap.head
        val product = ring.multiply(ax, ay)
        if (product == ring.zero) {
          zero
        } else {
          unsafeWrap(Map(monoid.add(bx, by) -> product))
        }
      } else {
        unsafeWrap((for ((by, ay) <- y.toSeq; product = ring.multiply(ax, ay); if product != ring.zero) yield (monoid.add(bx, by) -> product)).toMap)
      }
    } else {
      wrap((for ((bx, ax) <- x.toSeq; (by, ay) <- y.toSeq) yield (monoid.add(bx, by) -> ring.multiply(ax, ay))).groupBy(_._1).mapValues(s => ring.sum(s.map(_._2))))
    }
  }
  def one = unsafeWrap(Map(monoid.zero -> ring.one))

  def monomial(b: B): M = unsafeWrap(Map(b -> ring.one))
  def monomial(b: B, a: A): M = wrap(Map(b -> a))

  def constant(a: A): M = monomial(monoid.zero, a)
  override def fromInt(x: Int) = constant(ring.fromInt(x))
}
trait MapFreeModuleOnMonoid[A, B, M <: MapLinearCombo[A, B]] extends MapFreeModuleOnMonoidOverRig[A, B, M] with MapFreeModule[A, B, M] with AssociativeAlgebra[A, M] {
}

trait GeneralFreeModule[A, B, LC <: LinearCombo[A, B]] extends GeneralFreeModuleOverRig[A, B, LC] with Module[A, LC] {
  override implicit def ring: Ring[A]

  override def negate(x: LC) = {
    import AlgebraicNotation._
    simplify(x.terms map { case (b, a) => (b, -a) })
  }
}

trait FreeModule[A, B] extends GeneralFreeModule[A, B, LinearCombo[A, B]]

trait IntegerFreeModule[B, LC <: LinearCombo[Int, B]] extends GeneralFreeModule[Int, B, LC] {
  val ring = Integers
}

trait FreeModuleOnMonoidOverRig[A, B, LC <: LinearCombo[A, B]] extends GeneralFreeModuleOverRig[A, B, LC] with Rig[LC] {
  def monoid: AdditiveMonoid[B]

  def multiply(x: LC, y: LC) = {
    simplify(for ((bx, ax) <- x.terms; (by, ay) <- y.terms) yield (monoid.add(bx, by) -> ring.multiply(ax, ay)))
  }
  def one = wrap(Map(monoid.zero -> ring.one))

  def monomial(b: B): LC = wrap(Map(b -> ring.one))
  def monomial(b: B, a: A): LC = wrap(Map(b -> a))

  def constant(a: A): LC = monomial(monoid.zero, a)
  override def fromInt(x: Int) = constant(ring.fromInt(x))
}
trait FreeModuleOnMonoid[A, B, LC <: LinearCombo[A, B]] extends FreeModuleOnMonoidOverRig[A, B, LC] with GeneralFreeModule[A, B, LC] with Ring[LC] {
}

trait FancyFreeModule[A, B, LC <: LinearCombo[A, B], K <: Ordered[K]] extends GeneralFreeModule[A, B, LC] {
  def invariant: B => K
  def equivalence: (B, B) => Boolean

  import net.tqft.toolkit.collections.GroupBy._
  override def collect(x: Iterable[(B, A)]) = (x.equivalenceClasses((p1, p2) => equivalence(p1._1, p2._1), p => invariant(p._1)) map { l => (l.head._1 -> l.map(p => p._2)) })
}