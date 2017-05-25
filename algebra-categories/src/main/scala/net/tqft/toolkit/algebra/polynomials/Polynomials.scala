//package net.tqft.toolkit.algebra.polynomials
//
//import net.tqft.toolkit.algebra._
//import net.tqft.toolkit.algebra.modules._
//import net.tqft.toolkit.algebra.categories._
//
//object Polynomials extends HomomorphismCategory[PolynomialAlgebra] {
//
//  val over = new Functor[Ring, Ring, Polynomial] { self =>
//    def source = Rings
//    def target = Rings
//
//    def apply[A](_ring: OrderedField[A]): PolynomialAlgebraOverOrderedField[A] = new PolynomialAlgebraOverOrderedField[A] {
//      override val ring = _ring
//    }
//
//    def apply[A](_ring: Field[A]): PolynomialAlgebraOverField[A] = new PolynomialAlgebraOverField[A] {
//      override val ring = _ring
//    }
//
//    def apply[A](_ring: Ring[A]): PolynomialAlgebra[A] = new PolynomialAlgebra[A] {
//      override val ring = _ring
//    }
//
//    def apply[A, B](hom: Homomorphism[Ring, A, B]): Homomorphism[Ring, Polynomial[A], Polynomial[B]] = new RingHomomorphism[Polynomial[A], Polynomial[B]] {
//      def source = self.apply(hom.source)
//      def target = self.apply(hom.target)
//      def apply(p: Polynomial[A]): Polynomial[B] = new Polynomial[B] {
//        def terms = p.terms map { case (k: Int, a) => (k, hom(a)) }
//      }
//    }
//  }
//
//  def evaluationAt[A](x: A)(implicit ring: Ring[A]) = new Homomorphism[Ring, Polynomial[A], A] {
//    def source: Ring[Polynomial[A]] = over[A](ring)
//    def target: Ring[A] = ring
//    def apply(p: Polynomial[A]) = ring.sum(p.terms map { case (e, a) => ring.multiply(a, ring.power(x, e)) })
//  }
//
//  val embeddingAsConstants = new NaturalTransformation[Ring, Ring, Functors.Identity, Polynomial] {
//    def source = Functors.Identity(Rings)
//
//    def target = over
//
//    def apply[A](o: Ring[A]): Homomorphism[Ring, A, Polynomial[A]] = new Homomorphism[Ring, A, Polynomial[A]] {
//      def source = o
//      def target = over(o)
//      def apply(a: A) = Polynomial((0, a))(o)
//    }
//  }
//
//}