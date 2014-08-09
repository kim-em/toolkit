package net.tqft.toolkit.algebra.enumeration

object TriangleFreeGraphApp extends App {
  val n = 7

  val k = TriangleFreeGraph(6, Vector(List(1, 5), List(0, 4), List(3), List(2), List(1, 5), List(0, 4)))
  val kc = k.children
  for(c <- kc) println(c)
  for(i <- 0 until kc.size; j <- 0 until i) {
    println(isomorphismQ(kc(i), kc(j)))
  }
  
  val g0 = TriangleFreeGraph(1, IndexedSeq(Seq()))
  val graphsTree = g0.descendantsTree(n - _.numberOfVertices).toStream

  def isomorphismQ(a: TriangleFreeGraph, b: TriangleFreeGraph) = {
    import net.tqft.toolkit.permutations.Permutations
    a.numberOfVertices == b.numberOfVertices && {
      Permutations.of(a.numberOfVertices).exists(p => a.relabel(p) == b)
    }
  }

  for ((g, cs) <- graphsTree) {
    println(g)
    //    for(c <- cs) {
    //      require(isomorphismQ(g, c.parent.get))
    //    }
    //    require(g.verifyAncestry(isomorphismQ _))
  }

  val graphs = graphsTree.map(_._1)
  val graphsBySize = graphs.groupBy(_.numberOfVertices)

  for ((n, gs) <- graphsBySize; i <- 0 until gs.size; j <- 0 until i) {
    if (isomorphismQ(gs(i), gs(j))) {
      println(n)
      println(gs(i))
      println(gs(j))
      require(false)
    }
  }

  println(List(1, 2, 3, 7, 14, 38, 107, 410, 1897, 12172, 105071, 1262180, 20797002, 467871369))
  println(graphsBySize.mapValues(_.size).toSeq.sortBy(_._1).map(_._2))

}