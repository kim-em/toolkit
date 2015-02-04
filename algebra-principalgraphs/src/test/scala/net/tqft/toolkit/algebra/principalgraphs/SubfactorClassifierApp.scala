package net.tqft.toolkit.algebra.principalgraphs

object SubfactorClassifierApp extends App {
  import SubfactorClassifier._

  def p(w: SubfactorWeed) = {
    println(w)
  }

  var count = 0
  for ((g, children) <- A3(5.0).descendantsTreeFiltered(3, -1, 100, ignoring)) {
    count = count + 1
    //    p(g)
  }
  println(s"saw $count graphs")
}