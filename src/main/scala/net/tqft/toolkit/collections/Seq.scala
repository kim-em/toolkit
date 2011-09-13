package net.tqft.toolkit.collections

object Seq {
  implicit def seq2RichSeq[A](x: Seq[A]) = new RichSeq(x)

  class RichSeq[A](x: Seq[A]) {
    def get(i: Int): Option[A] = {
      if (i < 0) {
        None
      } else {
        x.drop(i).headOption
      }
    }
  }
}