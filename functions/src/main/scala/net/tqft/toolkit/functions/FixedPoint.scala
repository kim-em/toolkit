package net.tqft.toolkit.functions

import scala.language.implicitConversions

object FixedPoint {

  implicit def fixedPointForFunction[A](f: A => A) = new FunctionFixedPoint(f)

  class FunctionFixedPoint[A](f: A => A, equalityTest: (A, A) => Boolean = { (p: A, q: A) => p == q }) {
    def this(f: A => A, compareBy: A => Any) = this(f, { (p: A, q: A) => compareBy(p) == compareBy(q) })

    /**
     * Finds the fixed point of f. (Of course, if there isn't a fixed point it simply doesn't terminate.)
     */
    def fixedPoint(a: A): A = {
      var s = a
      var t = f(a)
      while(!equalityTest(s, t)) {
        s = f(t)
        if(equalityTest(s, t)) {
          return s
        } else {
          t = f(s)
        }
      }
      t
    }

    /**
     * Finds the first repeated value in the sequence of iterates a, f(a), f(f(a)), ..., and returns its value, position in the sequence, and the period.
     */
    def findFirstRepetition(a: A): (A, Int, Int) = {
      var values: List[A] = Nil
      var current = a
      var previousPosition: Int = 0
      while ({ previousPosition = values.indexOf({ b: A => equalityTest(b, current) }); previousPosition == -1 }) {
        values = current :: values
        current = f(current)
      }
      (current, values.size, previousPosition + 1)
    }

    /**
     * Finds the first repeated value in the sequence of iterates a, f(a), f(f(a)), ...
     */
    def firstRepeatedIterate(a: A): A = findFirstRepetition(a)._1
    /**
     * Finds the period of the eventual cycle in a, f(a), f(f(a)), ...
     */
    def eventualPeriod(a: A): Int = findFirstRepetition(a)._3
  }

  def apply[A](f: A => A): A => A = new FunctionFixedPoint(f).fixedPoint(_)
  def comparingBy[A, B](c: A => B)(f: A => A): A => A = new FunctionFixedPoint(f, c).fixedPoint(_)
  def withSameTest[A](e: (A, A) => Boolean)(f: A => A): A => A = new FunctionFixedPoint(f, e).fixedPoint(_)
  
  def firstRepeat[A](f: A => A): A => A = new FunctionFixedPoint(f).firstRepeatedIterate(_)
  def eventualPeriod[A](f: A => A): A => Int = new FunctionFixedPoint(f).eventualPeriod(_)
}