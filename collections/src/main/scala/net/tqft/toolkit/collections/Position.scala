package net.tqft.toolkit.collections

object Position {

  implicit def anyToPositionable(a: Any) = new Positionable(a)
  
  class Positionable(a: Any) {
    def position(pf: PartialFunction[Any, Boolean]): Seq[Seq[Int]] = {
      
      val matchesEntire = if(pf.isDefinedAt(a) && pf(a)) {
        List(List())
      } else {
        Nil
      }
      val matchesParts = a match {
        case a: Seq[_] => a.toSeq.zipWithIndex.map(p => p._1.position(pf).map(p._2 +: _)).flatten
        case _ => Nil
      }
      matchesEntire ++ matchesParts
    }
  }
  
}