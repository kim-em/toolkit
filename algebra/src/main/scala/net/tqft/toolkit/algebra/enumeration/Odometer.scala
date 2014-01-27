package net.tqft.toolkit.algebra.enumeration

import net.tqft.toolkit.Logging
import net.tqft.toolkit.collections.NonStrictIterable

trait Odometer[O] {
  def increment(): O
  def carry(): Option[O]
  def reset(): O
}

object Odometer extends Object with Logging {
  var counter = 0;

  private def successor[O](o: O)(implicit helper: O => Odometer[O], limit: O => Boolean): Option[O] = {
    val i = o.increment
    if (limit(i)) {
      Some(i)
    } else {
      carryRepeatedly(i)
    }
  }

  private def carryRepeatedly[O](o: O)(implicit helper: O => Odometer[O], limit: O => Boolean): Option[O] = {
    if (limit(o)) {
      Some(o)
    } else {
      o.carry match {
        case Some(c) => carryRepeatedly(c)
        case None => None
      }
    }
  }

  def apply[O](initial: O)(implicit helper: O => Odometer[O], limit: O => Boolean): Iterable[O] = {
    new NonStrictIterable[O] {
      def iterator = new Iterator[O] {

        var n: Option[O] = if (limit(initial)) {
          Some(initial)
        } else {
          successor(initial)
        }

        def next = {
          counter = counter + 1
          n match {
            case Some(o) => {
              n = successor(o)
              o
            }
            case None => throw new NoSuchElementException
          }
        }

        def hasNext = {
          n match {
            case Some(o) => true
            case None => false
          }
        }
      }
    }

  }
}

object ArrayOdometer {
  implicit def odometerInt(l: Array[Int]): Odometer[Array[Int]] = new o(l)

  private class o(l: Array[Int]) extends Odometer[Array[Int]] {
    require(l.nonEmpty)

    override def increment(): Array[Int] = {
      val m = l.clone
      m(0) = m(0) + 1
      m
    }
    override def carry(): Option[Array[Int]] = {
      // set the first nonzero entry to zero, increment the next entry
      l.indexWhere(_ > 0) match {
        case k if k == l.length - 1 => None
        case k => {
          val m = l.clone
          m(k) = 0
          m(k + 1) = m(k + 1) + 1
          Some(m)
        }
      }
    }
    override def reset = Array.fill(l.size)(0)
  }

}

object ListOdometer {
  implicit def odometerInt(l: List[Int]): Odometer[List[Int]] = new o(l)

  private class o(l: List[Int]) extends Odometer[List[Int]] {
    require(l.nonEmpty)

    override def increment(): List[Int] = {
      l.head + 1 :: l.tail
    }
    override def carry(): Option[List[Int]] = {
      // set the first nonzero entry to zero, increment the next entry
      l match {
        case 0 :: tail => (tail.carry) map { 0 :: _ }
        case a :: b :: tail => Some(0 :: (b + 1) :: tail)
        case _ => None
      }
    }
    override def reset = List.fill(l.size)(0)
  }

  implicit def odometerA[A](l: List[A])(implicit helper: A => Odometer[A]): Odometer[List[A]] = new o2(l)

  private class o2[A <% Odometer[A]](l: List[A]) extends Odometer[List[A]] {
    require(l.nonEmpty)

    override def increment(): List[A] = {
      (l.head.increment) :: l.tail
    }
    override def carry(): Option[List[A]] = {
      l match {
        case a :: tail if a == a.reset => (tail.carry) map { a :: _ }
        case a :: tail => a.carry match {
          case Some(aa) => Some(aa :: tail)
          case None => tail match {
            case Nil => None
            case _ => Some(a.reset :: tail.increment)
          }
        }
        case _ => None
      }
    }
    override def reset = l map { _.reset }
  }
}