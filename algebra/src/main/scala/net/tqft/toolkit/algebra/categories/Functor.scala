package net.tqft.toolkit.algebra.categories

object Functors {
  type Pair[A] = (A, A)
  type Identity[A] = A

  def Forget[O1[A] <: O2[A], O2[_]](cat1: HomomorphismCategory[O1], cat2: HomomorphismCategory[O2]): Functor[O1, O2, Identity] = new Functor[O1, O2, Identity] {
    def source = cat1
    def target = cat2
    def apply[A](o: O1[A]) = o
    def apply[A, B](hom: Homomorphism[O1, A, B]) = hom
  }

  def Identity[O[_]](cat: HomomorphismCategory[O]): Endofunctor[O, Identity] = new Endofunctor[O, Identity] {
    def source = cat
    def target = cat
    def apply[A](o: O[A]) = o
    def apply[A, B](hom: Homomorphism[O, A, B]) = hom
  }
}

trait Homomorphism[+O[_], A, B] extends (A => B) {
  def source: O[A]
  def target: O[B]
}
abstract class HomomorphismImpl[O[_], A:O, B:O] extends Homomorphism[O, A, B] {
  override def source = implicitly[O[A]]
  override def target = implicitly[O[B]]
}

trait TypedCategory[O[_], M[_, _]] {
  def identityMorphism[A](o: O[A]): M[A, A]
  def source[A, B](m: M[A, B]): O[A]
  def target[A, B](m: M[A, B]): O[B]
  def compose[A, B, C](x: M[A, B], y: M[B, C]): M[A, C]
}

trait GeneralHomomorphismCategory[O[_], H[A, B] <: Homomorphism[O, A, B]] extends TypedCategory[O, H] {
  def homomorphismFactory[A, B](source: O[A], target: O[B], function: A => B): H[A, B]

  def compose[A, B, C](x: H[A, B], y: H[B, C]) = homomorphismFactory(source = x.source, target = y.target, function = { a: A => y(x(a)) })
  def target[A, B](x: H[A, B]) = x.target
  def source[A, B](x: H[A, B]) = x.source
  def identityMorphism[A](o: O[A]) = homomorphismFactory(source = o, target = o, function = { a: A => a })
}

trait HomomorphismCategory[O[_]] extends GeneralHomomorphismCategory[O, ({ type H[A, B] = Homomorphism[O, A, B] })#H] {
  override def homomorphismFactory[A, B](_source: O[A], _target: O[B], function: A => B) = new Homomorphism[O, A, B] {
    val source = _source
    val target = _target
    def apply(a: A) = function(a)
  }
}

trait GeneralFunctor[O1[_], H1[A, B] <: Homomorphism[O1, A, B], O2[_], H2[A, B] <: Homomorphism[O2, A, B], Q[_]] { outer =>
  def source: GeneralHomomorphismCategory[O1, H1]
  def target: GeneralHomomorphismCategory[O2, H2]
  def apply[A](a: O1[A]): O2[Q[A]]
  def apply[A, B](hom: H1[A, B]): H2[Q[A], Q[B]]

  def andThen[O3[_], H3[A, B] <: Homomorphism[O3, A, B], P[_]](f: GeneralFunctor[O2, H2, O3, H3, P]): GeneralFunctor[O1, H1, O3, H3, ({ type PQ[A] = P[Q[A]] })#PQ] = {
    type PQ[A] = P[Q[A]]
    new GeneralFunctor[O1, H1, O3, H3, PQ] {
      def source = outer.source
      def target = f.target
      def apply[A](a: O1[A]) = f.apply(outer.apply(a))
      def apply[A, B](hom: H1[A, B]) = f.apply(outer.apply(hom))
    }
  }
}

trait Functor[O1[_], O2[_], Q[_]] extends GeneralFunctor[O1, ({ type Hom[A, B] = Homomorphism[O1, A, B] })#Hom, O2, ({ type Hom[A, B] = Homomorphism[O2, A, B] })#Hom, Q] {
}

trait GeneralEndofunctor[O[_], H[A, B] <: Homomorphism[O, A, B], Q[_]] extends GeneralFunctor[O, H, O, H, Q]

trait Endofunctor[O[_], Q[_]] extends GeneralEndofunctor[O, ({ type Hom[A, B] = Homomorphism[O, A, B] })#Hom, Q]

trait GeneralNaturalTransformation[O1[_], H1[B, C] <: Homomorphism[O1, B, C], O2[_], H2[B, C] <: Homomorphism[O2, B, C], P[_], Q[_]] {
  def source: GeneralFunctor[O1, H1, O2, H2, P]
  def target: GeneralFunctor[O1, H1, O2, H2, Q]
  def apply[A](o: O1[A]): H2[P[A], Q[A]]
}

trait NaturalTransformation[O1[_], O2[_], P[_], Q[_]] extends GeneralNaturalTransformation[O1, ({ type Hom[A, B] = Homomorphism[O1, A, B] })#Hom, O2, ({ type Hom[A, B] = Homomorphism[O2, A, B] })#Hom, P, Q] {
}

