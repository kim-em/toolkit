package net.tqft.toolkit.algebra.enumeration

import java.io.StringWriter
import java.io.PrintWriter
import scala.io.Source

object TriangleFreeGraphMergerApp extends App {
  val n = 4
  val m = 6

  val g0 = TriangleFreeGraph(1, IndexedSeq(Seq()))
  val g1 = TriangleFreeGraph(4, Vector(List(1), List(0, 3), List(), List(1)))
  
  def accept_n(g: TriangleFreeGraph) = n - g.numberOfVertices
  def accept_m(g: TriangleFreeGraph) = m - g.numberOfVertices

  val sw1 = new StringWriter()

  TreePrinter({ g: TriangleFreeGraph => g.toString }, { g: TriangleFreeGraph => g.numberOfVertices }, accept_n _)
    .to(sw1)
    .print(g0.descendants(accept_n _))

  val tree1 = sw1.toString

  val sw2 = new StringWriter()

  TreePrinter({ g: TriangleFreeGraph => g.toString }, { g: TriangleFreeGraph => g.numberOfVertices }, accept_m _)
    .to(sw2)
    .print(g1.descendants(accept_m _))

  val tree2 = sw2.toString

//  val sw3 = new StringWriter()
//  TreeMerger.merge(tree1, tree2, sw3)
//  println(sw3.toString)
}