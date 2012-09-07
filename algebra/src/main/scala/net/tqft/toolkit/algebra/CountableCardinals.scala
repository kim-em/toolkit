package net.tqft.toolkit.algebra

// TODO probably ditch this, it's not that interesting

trait CountableCardinal
object CountableCardinal {
  implicit def lift(i: Int) = FiniteCardinal(i)
}
case class FiniteCardinal(i: Int) extends CountableCardinal
case object CountableInfinity extends CountableCardinal

object CountableCardinals extends Rig[CountableCardinal] {
  override def zero = 0
  override def one = 1
  override def fromInt(x: Int) = x
  override def add(x: CountableCardinal, y: CountableCardinal) = {
    x match {
      case CountableInfinity => CountableInfinity
      case FiniteCardinal(i) => {
        y match {
          case CountableInfinity => CountableInfinity
          case FiniteCardinal(j) => FiniteCardinal(i + j)
        }
      }
    }
  }
  override def multiply(x: CountableCardinal, y: CountableCardinal) = {
    x match {
      case FiniteCardinal(0) => FiniteCardinal(0)
      case FiniteCardinal(i) => {
        y match {
          case CountableInfinity => CountableInfinity
          case FiniteCardinal(j) => FiniteCardinal(i * j)
        }
      }
      case CountableInfinity => {
        y match {
          case FiniteCardinal(0) => FiniteCardinal(0)
          case _ => CountableInfinity
        }
      }
    }
  }
}