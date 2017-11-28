package net.tqft.toolkit.algebra.principalgraphs

trait Memo[+T] {
  def value: T
  def map[S](f: T => S): Memo[S] = Memo(f(value))
  def computed: Boolean
  override def toString = "Memo(" + (if (computed) value.toString else "---not computed yet---") + ")"
  override def hashCode = value.hashCode
  override def equals(other: Any) = other match {
    case other: Memo[T] => value == other.value
    case _ => false
  }
}

object Memo {
  def apply[T](function: => T): Memo[T] = {
    new Memo[T] {
      private var _computed = false;
      override lazy val value = { val v = function; _computed = true; v }
      override def computed = _computed
    }
  }
}
