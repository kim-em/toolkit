package net.tqft.toolkit.algebra.enumeration

import java.io.StringWriter
import java.io.PrintWriter
import scala.io.Source

object TriangleFreeGraphPrinterApp extends App {
  val n = 7

  val g0 = TriangleFreeGraph(1, IndexedSeq(Seq()))
  def accept(g: TriangleFreeGraph) = n - g.numberOfVertices

  val sw = new StringWriter()

  TreePrinter({ g: TriangleFreeGraph => g.toString }, { g: TriangleFreeGraph => g.numberOfVertices }, accept _)
    .to(sw)
    .print(g0.descendants(accept _))

  println(sw)

  println("---")
  for (leaf <- TreeReader.readLeaves(Source.fromString(sw.toString).getLines)) println(leaf)
}