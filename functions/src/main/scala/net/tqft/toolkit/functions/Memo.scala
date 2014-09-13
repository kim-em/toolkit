package net.tqft.toolkit.functions

import scala.language.implicitConversions
import com.google.common.cache.CacheLoader
import com.google.common.cache.CacheBuilder
import scala.concurrent.Future

trait Memo {

  def apply[A, B](f: A => B): A => B = {
    apply(f, new com.google.common.collect.MapMaker().makeMap[A, B]())
  }

  def softly[A, B](f: A => B): A => B = {
    val loader =
      new CacheLoader[A with Object, B with Object]() {
        override def load(a: A with Object) = {
          f(a).asInstanceOf[B with Object]
        }
      }

    val cache = CacheBuilder.newBuilder().softValues()
      .build[A with Object, B with Object](loader)

    { a: A => cache.getUnchecked(a.asInstanceOf[A with Object]) }
  }

  def inBackground[A, B](f: A => B, backgroundCache: scala.collection.mutable.Map[A, B], foregroundCache: scala.collection.mutable.Map[A, B] = scala.collection.mutable.Map[A, B]()): A => B = new (A => B) {
    override def apply(a: A): B = {
      import scala.concurrent.ExecutionContext.Implicits.global
      if(foregroundCache.contains(a)) {
        foregroundCache(a)
      } else {
        if(backgroundCache.contains(a)) {
          val b = backgroundCache(a)
          foregroundCache.put(a, b)
          b
        } else {
          val b = f(a)
          foregroundCache.put(a, b)
          Future {
            backgroundCache.put(a, b)
          }
          b
        }
      }
    }
  }
  
  def apply[A, B](f: A => B, cache: scala.collection.mutable.Map[A, B]): A => B = new (A => B) {
    def apply(a: A) = {
      cache.getOrElseUpdate(a, f(a))
    }
  }
  def apply[A, B](f: A => B, cache: java.util.concurrent.ConcurrentMap[A, B]): A => B = new (A => B) {
    //    var hits = 0
    //    var misses = 0
    def apply(a: A) = {
      //      if(hits + misses % 100 == 0) println("hits: " + hits + " misses: " + misses)
      if (cache.containsKey(a)) {
        //        hits += 1
        cache.get(a)
      } else {
        //        misses += 1
        val result = f(a)
        cache.putIfAbsent(a, result)
        result
      }
    }
  }

  def apply[A, B](f: PartialFunction[A, B]): PartialFunction[A, B] = {
    apply(f, new com.google.common.collect.MapMaker().makeMap[A, Option[B]]())
  }

  def apply[A, B](f: PartialFunction[A, B], cache: java.util.concurrent.ConcurrentMap[A, Option[B]]): PartialFunction[A, B] = new PartialFunction[A, B] {
    private def getOrElseUpdate(a: A) = {
      if (cache.containsKey(a)) {
        cache.get(a)
      } else {
        val result = f.lift(a)
        cache.putIfAbsent(a, result)
        result
      }
    }
    override def isDefinedAt(a: A): Boolean = {
      getOrElseUpdate(a).isDefined
    }
    def apply(a: A) = {
      getOrElseUpdate(a).get
    }
  }

  def apply[A1, A2, B](f: (A1, A2) => B): (A1, A2) => B = {
    val m: ((A1, A2)) => B = apply({ p: (A1, A2) => f(p._1, p._2) });
    { (a1: A1, a2: A2) => m((a1, a2)) }
  }

  def softly[A1, A2, B <: Object](f: (A1, A2) => B): (A1, A2) => B = {
    val m: ((A1, A2)) => B = softly({ p: (A1, A2) => f(p._1, p._2) });
    { (a1: A1, a2: A2) => m((a1, a2)) }
  }

  def apply[A1, A2, B](f: (A1, A2) => B, cache: scala.collection.mutable.Map[(A1, A2), B]): (A1, A2) => B = new ((A1, A2) => B) {
    def apply(a1: A1, a2: A2) = {
      cache.getOrElseUpdate((a1, a2), f(a1, a2))
    }
  }
  def apply[A1, A2, B](f: (A1, A2) => B, cache: java.util.concurrent.ConcurrentMap[(A1, A2), B]): (A1, A2) => B = new ((A1, A2) => B) {
    def apply(a1: A1, a2: A2) = {
      if (cache.containsKey((a1, a2))) {
        cache.get((a1, a2))
      } else {
        val result = f(a1, a2)
        cache.putIfAbsent((a1, a2), result)
        result
      }
    }
  }

}

object Memo extends Memo {
  implicit class Memoable[A, B](f: A => B) {
    def memo = Memo(f)
    def memoSoftly = Memo.softly(f)
    def memoUsing(cache: scala.collection.mutable.Map[A, B]) = Memo(f, cache)
  }
}