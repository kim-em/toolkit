package net.tqft.toolkit.algebra

object AlgebraicNotation {
  def sum[A](i: Iterable[A])(implicit cm: CommutativeMonoid[A]) = i.foldLeft(cm.zero)(cm.add(_, _))

  implicit def commutativeMonoidElement[A](a: A)(implicit cm: CommutativeMonoid[A]) = new CommutativeMonoidElement(a, cm)
  class CommutativeMonoidElement[A](a: A, cm: CommutativeMonoid[A]) {
    def +(b: A) = cm.add(a, b)
  }

  implicit def ringElement[A](a: A)(implicit ring: Ring[A]) = new RingElement(a, ring)
  class RingElement[A](a: A, ring: Ring[A]) {
    def *(b: A) = ring.multiply(a, b)
    def ^(k: Int) = ring.power(a, k)
    def unary_-() = ring.negate(a)
  }

  implicit def euclideanDomainElement[A](a: A)(implicit domain: EuclideanDomain[A]) = new EuclideanDomainElement(a, domain)
  class EuclideanDomainElement[A](a: A, domain: EuclideanDomain[A]) {
    def %(b: A) = domain.remainder(a, b)
    def /(b: A) = domain.quotient(a, b)
  }
  
  implicit def moduleElement[A, B](b: B)(implicit module: Module[A, B]) = new ModuleElement(b, module)
  class ModuleElement[A, B](b: B, module: Module[A, B]) {
    def *:(a: A) = module.scalarMultiply(a, b)
  }

}