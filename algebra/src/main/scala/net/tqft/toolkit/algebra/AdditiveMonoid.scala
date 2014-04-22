package net.tqft.toolkit.algebra

import scala.collection.GenTraversableOnce

trait AdditiveSemigroup[@specialized(Int, Long, Float, Double) A] {
  def add(x: A, y: A): A
  final def add(x0: A, x1: A, x2: A*): A = x2.fold(add(x0, x1))(add _)
}


trait Zero[@specialized(Int, Long, Float, Double) A] {
  def zero: A
}

object Zero {
  implicit def forget[A: AdditiveMonoid]: Zero[A] = implicitly[Zero[A]]
}

trait AdditiveMonoid[@specialized(Int, Long, Float, Double) A] extends AdditiveSemigroup[A] with Zero[A] {
  def sum(xs: GenTraversableOnce[A]): A = xs.reduceOption(add _).getOrElse(zero)
}

trait AdditiveMonoidLowPriorityImplicits {
  implicit def forgetRig[A: Rig]: AdditiveMonoid[A] = implicitly[AdditiveMonoid[A]]
  implicit def forgetModule[A, B](implicit module: Module[A, B]): AdditiveMonoid[B] = module
}

object AdditiveMonoid extends AdditiveMonoidLowPriorityImplicits {
  implicit def forgetGroup[A: AdditiveGroup]: AdditiveMonoid[A] = implicitly[AdditiveMonoid[A]]

  trait AdditiveMonoidMap[A, B] extends AdditiveMonoid[Map[A, B]] {
    def coefficients: AdditiveMonoid[B]

    override def add(m1: Map[A, B], m2: Map[A, B]): Map[A, B] = {
      val newMap = scala.collection.mutable.Map[A, B]().withDefault(_ => coefficients.zero)
      for (m <- Seq(m1, m2); (a, b) <- m) {
        newMap(a) = coefficients.add(newMap(a), b)
      }
      Map() ++ newMap.filter({ case (_, v) => v != coefficients.zero })
    }

    override lazy val zero = Map[A, B]()
  }
  trait AdditiveMonoidSeq[B] extends AdditiveMonoid[Seq[B]] {
    def coefficients: AdditiveMonoid[B]

    override def zero: Seq[B] = Seq.empty
    override def add(s1: Seq[B], s2: Seq[B]): Seq[B] = {
      truncate(s1.zipAll(s2, coefficients.zero, coefficients.zero).map(p => coefficients.add(p._1, p._2)))
    }
    
    def truncate(s: Seq[B]): Seq[B] = {
      val k = s.lastIndexWhere({ b: B => b != coefficients.zero })
      if(k == -1) {
        Seq.empty
      } else {
        s.take(k + 1)
      }
    }
  }

  implicit def additiveMonoidMap[A, B: AdditiveMonoid]: AdditiveMonoid[Map[A, B]] = new AdditiveMonoidMap[A, B] {
    override def coefficients = implicitly[AdditiveMonoid[B]]
  }
  implicit def additiveMonoidSeq[B: AdditiveMonoid]: AdditiveMonoid[Seq[B]] = new AdditiveMonoidSeq[B] {
    override def coefficients = implicitly[AdditiveMonoid[B]]
  }
}