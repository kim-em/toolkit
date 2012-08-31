package net.tqft.toolkit.algebra

object AlgebraicNotation {
  implicit def summable[A: CommutativeMonoid](i: Iterable[A]): Summable[A] = new Summable(i)
  class Summable[A: CommutativeMonoid](i: Iterable[A]) {
    def sum = {
      val cm = implicitly[CommutativeMonoid[A]]
      i.foldLeft(cm.zero)(cm.add(_, _))
    }
  }

  implicit def commutativeMonoidElement[A: CommutativeMonoid](a: A) = new CommutativeMonoidElement(a)
  class CommutativeMonoidElement[A: CommutativeMonoid](a: A) {
    def +(b: A) = implicitly[CommutativeMonoid[A]].add(a, b)
  }
  implicit def commutativeGroupElement[A: CommutativeGroup](a: A) = new CommutativeGroupElement(a)
  class CommutativeGroupElement[A: CommutativeGroup](a: A) {
    def group = implicitly[CommutativeGroup[A]]
    def -(b: A) = group.subtract(a, b)
    def unary_-() = group.negate(a)
  }

  implicit def ringElement[A: Ring](a: A) = new RingElement(a)
  class RingElement[A: Ring](a: A) {
    private def ring = implicitly[Ring[A]]

    def *(b: A) = ring.multiply(a, b)
    def ^(k: Int) = ring.power(a, k)
  }

  implicit def euclideanDomainElement[A: EuclideanDomain](a: A) = new EuclideanDomainElement(a)
  class EuclideanDomainElement[A: EuclideanDomain](a: A) {
    private def domain = implicitly[EuclideanDomain[A]]
    def %(b: A) = domain.remainder(a, b)
    def /(b: A) = domain.quotient(a, b)
  }

  implicit def moduleElement[A, B](b: B)(implicit module: Module[A, B]) = new ModuleElement(b)
  class ModuleElement[A, B](b: B)(implicit module: Module[A, B]) {
    def *:(a: A) = module.scalarMultiply(a, b)
  }

}