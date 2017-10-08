package net.tqft.toolkit.algebra.mtc

import java.io.File
import scala.io.Source
import net.tqft.toolkit.algebra.graphs.ColouredGraph
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import scala.collection.Seq

object ModularDataIsomorphism extends App {
  // returns S and T matrices, read from a file in Angus' GAP output format.
  def parseFile(file: File): (Array[Array[String]], Array[String]) = {
    val lines = Source.fromFile(file).getLines.toStream.tail.init
    def parseMatrix(s: String) = s.replaceAllLiterally("[", "").replaceAllLiterally("]", "").replaceAllLiterally(" ", "").split(",")
    val T = parseMatrix(lines(1).drop(5).dropRight(1))
    val S = parseMatrix(lines(0).drop(5).dropRight(1)).grouped(T.size).toArray
    (S, T)
  }

  def nameString(ms: Seq[(Array[Array[String]], Array[String])]): ((Int, Int), Seq[(Array[Array[Int]], Array[Int])]) = {
    val S_values = ms.map(_._1).flatten.flatten.distinct
    val T_values = ms.map(_._2).flatten.distinct
    val STs = for (m <- ms) yield {
      val S = m._1.map(r => r.map(s => S_values.indexOf(s)))
      val T = m._2.map(x => T_values.indexOf(x))
      (S, T)
    }
    ((S_values.size, T_values.size), STs)
  }

  def constructGraph(s: Int, t: Int, m: (Array[Array[Int]], Array[Int])): ColouredGraph[(String, Int)] = {
    val S = m._1
    val T = m._2
    val rank = T.length
    val adjacencies = IndexedSeq.fill(rank)(Nil) ++ (for (i <- 0 until rank; j <- i until rank) yield Seq(i, j).distinct) ++ IndexedSeq.fill(s + t)(Nil)
    val colours = (for (i <- 0 until rank) yield ("A", T(i))) ++ (for (i <- 0 until rank; j <- i until rank) yield ("B", S(i)(j))) ++ IndexedSeq.tabulate(t)(i => ("A", i)) ++ IndexedSeq.tabulate(s)(i => ("B", i))
    val r = ColouredGraph(rank + (rank + 1) * rank / 2 + s + t, adjacencies, colours)
//    println(r.toDreadnautString)
    r
  }

  def isomorphism(file1: File, file2: File): Option[IndexedSeq[Int]] = {
    val ((s, t), Seq(m1, m2)) = nameString(Seq(parseFile(file1), parseFile(file2)))

    val rank = m1._2.length

    val G1 = constructGraph(s, t, m1)
    val G2 = constructGraph(s, t, m2)

    Dreadnaut.findIsomorphism(G1, G2, separateProcess = true).map(i => i.take(rank))
  }

  val file1 = new File(args(0))
  val file2 = new File(args(1))

  isomorphism(file1, file2) match {
    case Some(i) => println("found isomorphism: " + i.mkString(" "))
    case None => println("not isomorphic")
  }
}