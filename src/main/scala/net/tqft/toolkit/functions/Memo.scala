package net.tqft.toolkit.functions
import net.tqft.toolkit.Logging

object Memo extends Logging {

  def apply[A, B](f: A => B): A => B = apply(f, scala.collection.mutable.Map[A, B]())
  
  def apply[A, B](f: A => B, cache: scala.collection.mutable.Map[A, B]): A => B = new (A => B) {
    def apply(a: A) = {
      cache.getOrElseUpdate(a, f(a))
    }
  }
  
  def apply[A1, A2, B](f: (A1, A2) => B): (A1, A2) => B = apply(f, scala.collection.mutable.Map[(A1, A2), B]())
  
  def apply[A1, A2, B](f: (A1, A2) => B, cache: scala.collection.mutable.Map[(A1, A2), B]): (A1, A2) => B = new ((A1, A2) => B) {
    def apply(a1: A1, a2: A2) = {
      cache.getOrElseUpdate((a1, a2), f(a1, a2))
    }
  }
}