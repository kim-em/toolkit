package net.tqft.toolkit

object DeleteOne {

  implicit def deleteOneable[A](x: List[A]) = new DeleteOne(x)

  class DeleteOne[A](x: List[A]) {
    def deleteOne(y: A) = {
      x.indexOf(y) match {
        case -1 => throw new NoSuchElementException
        case k => x.take(k) ::: x.drop(k + 1)
      }
    }
  }

}