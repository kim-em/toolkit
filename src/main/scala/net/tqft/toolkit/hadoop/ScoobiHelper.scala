package net.tqft.toolkit.hadoop

import com.nicta.scoobi.Scoobi._

object ScoobiHelper {
  implicit def asHitherable[A](dlist: DList[A]) = new Hitherable(dlist)

  class Hitherable[A](dlist: DList[A]) {
    def hither: Iterable[A] = {
      val m = dlist.materialize
      val job = Job()
      job << m.use
      job.run()
      m.get
    }
  }
}