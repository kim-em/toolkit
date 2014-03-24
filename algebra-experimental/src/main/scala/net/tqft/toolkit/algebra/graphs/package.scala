package net.tqft.toolkit.algebra

package object graphs {
  implicit class RichSeq[A](x: Seq[A]) {
    def most = x.dropRight(1)
    def removed(i: Int) = x.take(i) ++ x.drop(i + 1)
    def secondLast = x(x.size - 2)
  }
}