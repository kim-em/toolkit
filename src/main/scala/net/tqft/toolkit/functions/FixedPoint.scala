package net.tqft.toolkit.functions

import net.tqft.toolkit.collections.NonStrictNaturalNumbers

object FixedPoint {

  implicit def fixedPointForFunction[A](f: A => A) = new FunctionFixedPoint(f)

  class FunctionFixedPoint[A](f: A => A, equalityTest: (A, A) => Boolean = { (p: A, q: A) => p == q }) {
    def this(f: A => A, compareBy: A => Any) = this(f, { (p: A, q: A) => compareBy(p) == compareBy(q) })
    
    def fixedPoint(a: A): A = {
      val iterates = Stream.iterate(a)(f)
      ((iterates zip iterates.tail) find { case (a, b) => equalityTest(a, b) }).get._2
    }
    def firstRepeatedIteration(a: A) = {
      val iterates = Stream.iterate(a)(f)
      iterates(NonStrictNaturalNumbers.find({ n => iterates.take(n).toList.find({b:A => equalityTest(iterates(n), b)}).nonEmpty}).get)
    }
  }

  def apply[A](f: A => A): A => A = f.fixedPoint(_)
  def comparingBy[A, B](c: A => B)(f: A => A): A => A = new FunctionFixedPoint(f, c).fixedPoint(_)
  def sameTest[A](e: (A, A) => Boolean)(f: A => A): A=>A = new FunctionFixedPoint(f,e).fixedPoint(_)
}