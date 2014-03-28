package net.tqft.toolkit.collections

import scala.language.implicitConversions

object Position {

  implicit class Positionable(a: Any) {
    def position(pf: PartialFunction[Any, Boolean]): Iterator[Seq[Int]] = {
      
      val matchesEntire = if(pf.isDefinedAt(a) && pf(a)) {
        Iterator(List())
      } else {
        Iterator.empty
      }
      val matchesParts = a match {
        case a: Seq[_] => a.toSeq.zipWithIndex.map(p => p._1.position(pf).map(p._2 +: _)).flatten
        case _ => Nil
      }
      matchesEntire ++ matchesParts
    }
  }
  
}