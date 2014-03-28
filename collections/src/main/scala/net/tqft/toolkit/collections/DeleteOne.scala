package net.tqft.toolkit.collections

object DeleteOne {

  implicit class DeleteOne[A](x: Seq[A]) {
    def deleteOne(y: A) = {
      x.indexOf(y) match {
        case -1 => throw new NoSuchElementException
        case k => x.take(k) ++ x.drop(k + 1)
      }
    }
    def deleteAtMostOne(y: A) = {
      x.indexOf(y) match {
        case -1 => x
        case k => x.take(k) ++ x.drop(k + 1)
      }
      
    }
  }

}