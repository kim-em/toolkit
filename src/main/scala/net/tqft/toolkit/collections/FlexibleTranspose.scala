package net.tqft.toolkit.collections

import scala.collection.mutable.ListBuffer
object FlexibleTranspose {
  implicit def flexibleTranspose[A](listOfLists: List[List[A]]) = new FlexibleTranspose(listOfLists)

  class FlexibleTranspose[A](listOfLists: List[List[A]]) {
    def flexibleTranspose: List[List[A]] = {
      val builders = Stream.continually(new ListBuffer[A]())
      for (list <- listOfLists) {
        for((b, a) <- builders zip list) {
          b += a
        }
      }
      builders.takeWhile(_.nonEmpty) map (_.toList) toList
    }
  }

}