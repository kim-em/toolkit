package net.tqft.toolkit.algebra

trait ApproximateField[A] extends OrderedField[A] {
  /* a small quantity, but 1 and 1+epsilon are still distinguishable */
  def epsilon: A
  def chop(x: A): A = chop(x, epsilon)
  def close(x: A, y: A) = {
    if (chop(x) == zero) {
      chop(y) == zero
    } else if (chop(y) == zero) {
      chop(x) == zero
    } else {
      chop(subtract(quotient(x, y), one)) == zero
    }
  }

  def sqrt(x: A): A = {
    import net.tqft.toolkit.functions.FixedPoint
    val initialGuess = one
    val result = abs(FixedPoint.withSameTest(close _)({ g: A => quotient(add(quotient(x, g), g), fromInt(2)) })(initialGuess))
    result
  }

  import collection.generic.CanBuildFrom
  import collection.TraversableLike

  def norm[CC[X] <: TraversableLike[X, CC[X]]](v: CC[A]): A = {
    sqrt(v.fold(zero)({ (s, a) => add(s, power(a, 2)) }))
  }

  def normalize[CC[X] <: TraversableLike[X, CC[X]]](v: CC[A])(implicit cbf1: CanBuildFrom[CC[A], Int, CC[Int]], cbf2: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val s = v.map(signum(_)).filter(_ != 0).head
    val n = multiplyByInt(norm(v), s)
    v map { x => quotient(x, n) }
  }

}