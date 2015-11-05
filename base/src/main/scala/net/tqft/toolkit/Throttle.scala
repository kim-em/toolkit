package net.tqft.toolkit

import scala.language.higherKinds


object Throttle extends Logging {
  object none extends Throttle { def apply(success: Boolean) {} }
  def linearBackoff(stepInMilliseconds: Int) = new Throttle {
    var delay = 0
    def apply(success: Boolean) = {
      if (success) {
        delay = 0
      } else {
        delay = delay + stepInMilliseconds
        info("throttling, " + delay + "ms")
        Thread.sleep(delay)
      }
    }
  }
  def rateLimited(rateInMilliseconds: Int) = new Throttle {
    var lastFailure = System.currentTimeMillis() - rateInMilliseconds
    def apply(success: Boolean) = {
      if (!success) {
        val diff = System.currentTimeMillis() - lastFailure
        lastFailure = lastFailure + diff
        if (diff < rateInMilliseconds) { 
          info("throttling, " + (rateInMilliseconds - diff) + "ms")        
          Thread.sleep(rateInMilliseconds - diff)
        }
      }
    }
  }

  def iterable2ThrottledIterable[A, CC[X] <: Traversable[X]](iterable: Iterable[CC[A]]) = new ThrottledIterable[A, CC](iterable)

  class ThrottledIterable[A, CC[X] <: Traversable[X]](iterable: Iterable[CC[A]]) {
    def flattenWithThottle(throttle: Throttle) = iterable.flatMap { t => { throttle(t.nonEmpty); t } }
  }
}

trait Throttle {
  def apply(success: Boolean)
}