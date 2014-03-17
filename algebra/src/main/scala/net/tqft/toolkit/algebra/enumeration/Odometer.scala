package net.tqft.toolkit.algebra.enumeration

import net.tqft.toolkit.Logging
import net.tqft.toolkit.collections.NonStrictIterable

trait Odometer[O] {
  def increment(o: O): O
  def carry(o: O): Option[O]
  def reset(o: O): O
}

object Odometer extends Object with Logging {
  //  var counter = 0;

  def apply[O: Odometer](limit: O => Boolean)(initial: O): Iterator[O] = {
    implicit val odometer = implicitly[Odometer[O]]
    import odometer._

    def successor(o: O): Option[O] = {
      val i = increment(o)
      if (limit(i)) {
        Some(i)
      } else {
        carryRepeatedly(i)
      }
    }

    def carryRepeatedly(o: O): Option[O] = {
      if (limit(o)) {
        Some(o)
      } else {
        carry(o) match {
          case Some(c) => carryRepeatedly(c)
          case None => None
        }
      }
    }

    new Iterator[O] {

      var n: Option[O] = if (limit(initial)) {
        Some(initial)
      } else {
        successor(initial)
      }

      def next: O = {
        //          counter = counter + 1
        n match {
          case Some(o) => {
            n = successor(o)
            o
          }
          case None => throw new NoSuchElementException
        }
      }

      def hasNext: Boolean = {
        n match {
          case Some(o) => true
          case None => false
        }
      }
    }

  }

  implicit object IntArrayOdometer extends Odometer[Array[Int]] {
    override def increment(l: Array[Int]): Array[Int] = {
      val m = l.clone
      m(0) = m(0) + 1
      m
    }
    override def carry(l: Array[Int]): Option[Array[Int]] = {
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
    override def reset(l: Array[Int]) = Array.fill(l.size)(0)

  }

  implicit object IntListOdometer extends Odometer[List[Int]] {
    override def increment(l: List[Int]): List[Int] = {
      l.head + 1 :: l.tail
    }
    override def carry(l: List[Int]): Option[List[Int]] = {
      // set the first nonzero entry to zero, increment the next entry
      l match {
        case 0 :: tail => (carry(tail)) map { 0 :: _ }
        case a :: b :: tail => Some(0 :: (b + 1) :: tail)
        case _ => None
      }
    }
    override def reset(l: List[Int]) = List.fill(l.size)(0)

  }

  implicit def ListOdometer[A: Odometer] = new ListOdometer[A]

  class ListOdometer[A: Odometer] extends Odometer[List[A]] {
    val aOdometer = implicitly[Odometer[A]]

    override def increment(l: List[A]): List[A] = {
      (aOdometer.increment(l.head)) :: l.tail
    }
    override def carry(l: List[A]): Option[List[A]] = {
      l match {
        case a :: tail if a == aOdometer.reset(a) => carry(tail) map { a :: _ }
        case a :: tail => aOdometer.carry(a) match {
          case Some(aa) => Some(aa :: tail)
          case None => tail match {
            case Nil => None
            case _ => Some(aOdometer.reset(a) :: increment(tail))
          }
        }
        case _ => None
      }
    }
    override def reset(l: List[A]) = l map { aOdometer.reset }

  }
}