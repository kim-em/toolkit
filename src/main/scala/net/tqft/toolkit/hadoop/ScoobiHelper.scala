package net.tqft.toolkit.hadoop

import com.nicta.scoobi.Scoobi._
import net.tqft.toolkit.Logging

object ScoobiHelper {
  implicit def asHitherable[A](dlist: DList[A]) = new Hitherable(dlist)

  class Hitherable[A](dlist: DList[A]) {
    def hither: Iterable[A] = {
      val m = dlist.materialize
      val job = Job()
      job << m.use
      try {
        job.run()
        m.get
      } catch {
        case e: Exception => {
          Logging.error("Caught an exception during 'hither': ", e)
          Iterable.empty
        }
      }
    }
  }
}