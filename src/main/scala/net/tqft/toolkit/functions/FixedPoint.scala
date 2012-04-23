package net.tqft.toolkit.functions

import net.tqft.toolkit.collections.NonStrictNaturalNumbers

object FixedPoint {

  implicit def fixedPointForFunction[A](f: A => A) = new FunctionFixedPoint(f)

  class FunctionFixedPoint[A](f: A => A, equalityTest: (A, A) => Boolean = { (p: A, q: A) => p == q }) {
    def this(f: A => A, compareBy: A => Any) = this(f, { (p: A, q: A) => compareBy(p) == compareBy(q) })

    def fixedPoint(a: A): A = {
      val twoValues = scala.collection.mutable.ListBuffer(a, f(a))
      var i = 0
      while (!equalityTest(twoValues(0), twoValues(1))) {
        twoValues(i) = f(twoValues(1 - i))
        i = (i + 1) % 2
      }
      twoValues(1 - i)
    }
    def firstRepeatedIteration(a: A) = {
      val iterates = Stream.iterate(a)(f)
      iterates(NonStrictNaturalNumbers.find({ n => iterates.take(n).toList.find({ b: A => equalityTest(iterates(n), b) }).nonEmpty }).get)
    }
  }

  def apply[A](f: A => A): A => A = f.fixedPoint(_)
  def comparingBy[A, B](c: A => B)(f: A => A): A => A = new FunctionFixedPoint(f, c).fixedPoint(_)
  def sameTest[A](e: (A, A) => Boolean)(f: A => A): A => A = new FunctionFixedPoint(f, e).fixedPoint(_)
}