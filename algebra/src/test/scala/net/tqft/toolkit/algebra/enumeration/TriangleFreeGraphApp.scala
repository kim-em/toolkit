package net.tqft.toolkit.algebra.enumeration

object TriangleFreeGraphApp extends App {
    val g0 = TriangleFreeGraph(1, IndexedSeq(Seq()))
    val graphs = g0.descendants(10 - _.numberOfVertices)
    for (g <- graphs) println(g)
}