package net.tqft.toolkit.algebra

import scala.collection.GenTraversableOnce

// TODO should have
//		@specialized(Int, Long, Float, Double) 
// but this crashes the compiler (somewhere in FiniteGroups??).
// Fixed by Paul Phillips, c.f. https://issues.scala-lang.org/browse/SI-6301 but hasn't hit 2.10 yet.
trait AdditiveSemigroup[A] {
  def add(x: A, y: A): A
  final def add(x0: A, x1: A, x2: A*): A = x2.fold(add(x0, x1))(add _)
}

// TODO should have
//	    @specialized(Int, Long, Float, Double) 
// but this causes Gadgets.Integers.zero to stack overflow!
// Also fixed by Paul, see above.
trait Zero[A] {
  def zero: A
}

trait AdditiveMonoid[@specialized(Int, Long, Float, Double) A] extends AdditiveSemigroup[A] with Zero[A] {
  def sum(xs: GenTraversableOnce[A]): A = xs.reduceOption(add _).getOrElse(zero)
}

trait AdditiveMonoidLowPriorityImplicits {
  implicit def forget[A:Rig]: AdditiveMonoid[A] = implicitly[AdditiveMonoid[A]]  
}

object AdditiveMonoid extends AdditiveMonoidLowPriorityImplicits {
  implicit def forget[A:AdditiveGroup]: AdditiveMonoid[A] = implicitly[AdditiveGroup[A]]
  
  class AdditiveMonoidMap[A, B:AdditiveMonoid] extends AdditiveMonoid[Map[A, B]] {
    def coefficients: AdditiveMonoid[B] = implicitly[AdditiveMonoid[B]]
        
    override def add(m1: Map[A, B], m2: Map[A, B]): Map[A, B] = {
      val newMap = scala.collection.mutable.Map[A,B]().withDefault(_ => coefficients.zero)
      for(m <- Seq(m1, m2); (a, b) <- m) {
        newMap(a) = coefficients.add(newMap(a), b)
      }
      Map() ++ newMap
    }
    
    override def zero = Map[A, B]()
  }
  class AdditiveMonoidSeq[B:AdditiveMonoid] extends AdditiveMonoid[Seq[B]] {
    def coefficients: AdditiveMonoid[B] = implicitly[AdditiveMonoid[B]]

    override def zero: Seq[B] = Seq.empty
    override def add(s1: Seq[B], s2: Seq[B]): Seq[B] = {
      s1.zipAll(s2, coefficients.zero, coefficients.zero).map(p => coefficients.add(p._1, p._2))
    }
  }
  
  implicit def additiveMonoidMap[A, B:AdditiveMonoid]: AdditiveMonoid[Map[A, B]] = new AdditiveMonoidMap[A, B]
  implicit def additiveMonoidSeq[B:AdditiveMonoid]: AdditiveMonoid[Seq[B]] = new AdditiveMonoidSeq[B]
}