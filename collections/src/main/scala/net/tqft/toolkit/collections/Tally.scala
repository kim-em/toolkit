package net.tqft.toolkit.collections

object Tally {

  implicit class Tallyable[A](i: Iterable[A]) {
    def tally: Seq[(A, Int)] = {
      i.groupBy(x => x).mapValues(_.size).toSeq
    }
  }

}