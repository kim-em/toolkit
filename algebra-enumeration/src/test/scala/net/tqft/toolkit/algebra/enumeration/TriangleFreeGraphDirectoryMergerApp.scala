package net.tqft.toolkit.algebra.enumeration

import java.io.StringWriter
import java.io.PrintWriter
import scala.io.Source
import java.io.FileOutputStream
import scala.io.StdIn

object TriangleFreeGraphDirectoryMergerApp extends App {
  val n = 4
  val m = 6

  val g0 = TriangleFreeGraph(1, IndexedSeq(Seq()))
  val g1 = TriangleFreeGraph(4, Vector(List(1), List(0, 3), List(), List(1)))
  val g2 = TriangleFreeGraph(5, Vector(List(1), List(0, 3), List(), List(1, 4), List(3)))

  def accept_n(g: TriangleFreeGraph) = n - g.numberOfVertices
  def accept_m(g: TriangleFreeGraph) = m - g.numberOfVertices

  val w0 = new PrintWriter(new FileOutputStream(g0 + ".tree"))

  TreePrinter({ g: TriangleFreeGraph => g.toString }, { g: TriangleFreeGraph => g.numberOfVertices }, accept_n _)
    .to(w0)
    .print(g0.descendants(accept_n _))

  println("Printed first tree to " + System.getProperty("user.dir"))

  val w1 = new PrintWriter(new FileOutputStream(g1 + ".tree"))

  TreePrinter({ g: TriangleFreeGraph => g.toString }, { g: TriangleFreeGraph => g.numberOfVertices }, accept_m _)
    .to(w1)
    .print(g1.descendants(accept_m _))

  println("Printed second tree")
  
  val w2 = new PrintWriter(new FileOutputStream(g2 + ".tree"))

  TreePrinter({ g: TriangleFreeGraph => g.toString }, { g: TriangleFreeGraph => g.numberOfVertices }, accept_m _)
    .to(w2)
    .print(g2.descendants(accept_m _))

  println("Printed third tree")
  println("Hit enter to merge")

  StdIn.readLine()

  TreeMerger.mergeDirectory()
}