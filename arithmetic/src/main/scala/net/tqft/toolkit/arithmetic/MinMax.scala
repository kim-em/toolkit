package net.tqft.toolkit.arithmetic

object MinMax {
  implicit class MinMaxOperations[A](c: TraversableOnce[A]) {
    def minOption(implicit cmp: Ordering[A]): Option[A] = if(c.isEmpty) {
      None
    } else {
      Some(c.min)
    }
    def minByOption[B: Ordering](f: A => B): Option[A] = if(c.isEmpty) {
      None
    } else {
      Some(c.minBy(f))
    }
    def maxOption(implicit cmp: Ordering[A]): Option[A] = if(c.isEmpty) {
      None
    } else {
      Some(c.max)
    }
    def maxByOption[B: Ordering](f: A => B): Option[A] = if(c.isEmpty) {
      None
    } else {
      Some(c.maxBy(f))
    }
  }
  
  implicit class MinMaxByOperations[A](c: TraversableOnce[A]) {
    def indexAndValueOfMinBy[B: Ordering](f: A => B): (Int, B) = {
      var bestSoFarIndex = -1
      var bestSoFar: Option[B] = None
      var k = 0
      for(x <- c) {
        val fx = f(x)
        bestSoFar match {
          case None => {
            bestSoFarIndex = k
            bestSoFar = Some(fx)
          }
          case Some(b) if implicitly[Ordering[B]].compare(fx, b) < 0 => {
            bestSoFarIndex = k
            bestSoFar = Some(fx)            
          } 
          case _ =>
        }
      }
    (bestSoFarIndex, bestSoFar.get)
    }
  }
}