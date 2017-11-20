package net.tqft.toolkit.algebra.mtc

import java.io.File
import scala.io.Source
import net.tqft.toolkit.algebra.graphs.ColouredGraph
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import scala.collection.Seq
import scala.collection.mutable.ListBuffer
import java.io.FileOutputStream
import java.io.PrintWriter
import net.tqft.toolkit.algebra.graphs.MatricesUpToSimultaneousRowAndColumnPermutations
import org.apache.commons.io.filefilter.WildcardFileFilter
import java.nio.file.Files
import java.nio.file.Paths
import net.tqft.toolkit.functions.Memo

/*
 * Brendan suggests we could speed this up by using the sparse graph version of nauty:
 *  "Use the sparse version of nauty.  Note that it makes a different canonical form than the default dense version, so you can't mix them.
 *  "Also, use "-a-m" unless you need the automorphisms, to save a tiny bit.
 *
 * Also, we should be computing canonical forms, not doing pairwise isomorphism checking!!
 */

object ModularDataIsomorphism extends App {
  

//  def constructGraph(s: Int, t: Int, m: (List[List[Int]], List[Int])): ColouredGraph[(String, Int)] = {
//    val S = m._1
//    val T = m._2
//    val rank = T.length
//    val adjacencies = IndexedSeq.fill(rank)(Nil) ++ (for (i <- 0 until rank; j <- i until rank) yield Seq(i, j).distinct) ++ IndexedSeq.fill(s + t)(Nil)
//    val colours = (for (i <- 0 until rank) yield ("A", T(i))) ++ (for (i <- 0 until rank; j <- i until rank) yield ("B", S(i)(j))) ++ IndexedSeq.tabulate(t)(i => ("A", i)) ++ IndexedSeq.tabulate(s)(i => ("B", i))
//    val r = ColouredGraph(rank + (rank + 1) * rank / 2 + s + t, adjacencies, colours)
//    //    println(r.toDreadnautString)
//    r
//  }

  var errorCounter = 0

  
  
  def isomorphism(file1: File, file2: File): Option[IndexedSeq[Int]] = {
    val p1 = ModularDataLoader.parseFile(file1)
    val p2 = ModularDataLoader.parseFile(file2)

    if (p1._2.sorted.toList != p2._2.sorted.toList) return None
    if (p1._1.flatten.sorted.toList != p2._1.flatten.sorted.toList) return None

    try {
      val result = MatricesUpToSimultaneousRowAndColumnPermutations.isomorphism(ModularDataLoader.superimposedST(file1), ModularDataLoader.superimposedST(file2))
      errorCounter = 0
      result
    } catch {
      case e: Exception => {
        println("error while checking for isomorphisms:")
        println(file1)
        println(file2)
        errorCounter = errorCounter + 1
        if (errorCounter < 5) {
          e.printStackTrace
          isomorphism(file1, file2)
        } else {
          throw e
        }
      }
    }
    //
    //    val G1 = constructGraph(s, t, m1)
    //    val G2 = constructGraph(s, t, m2)
    //
    //    Dreadnaut.findIsomorphism(G1, G2, separateProcess = true).map(i => i.take(rank))
  }

  def file(order: Int, p: (Int, Int)): File = {
      new File("../modular-data/code/Modular_Data/" + order + "/" + p._1 + "/" + p._2 + ".txt")
    }
  
  def isomorphism(order: Int, p1: (Int, Int), p2: (Int, Int)): Option[IndexedSeq[Int]] = {
    isomorphism(file(order, p1), file(order, p2))
  }

  def findIsomorphismClases(order: Int, chunk: Seq[(Int, Int)]): Seq[Seq[(Int, Int)]] = {
    println("looking for isomorphisms within chunk: " + chunk)
    val classes = ListBuffer[ListBuffer[(Int, Int)]]()
    for (p <- chunk) {
      classes.find(c => isomorphism(order, c.head, p).nonEmpty) match {
        case Some(c) => c += p
        case None    => classes += ListBuffer[(Int, Int)](p)
      }
    }
    println("found isomorphism classes: " + classes.toSeq.map(_.toSeq))
    classes
  }

  def parseOrbits_txt(order: Int): Seq[Seq[(Int, Int)]] = {
    val file = new File("../modular-data/code/Modular_Data/" + order + "/PossibleOrbits.txt")
    if (file.exists) {
      Source
        .fromFile(file)
        .getLines
        .mkString("\n")
        .stripPrefix("[")
        .stripSuffix("]")
        .split("\\]\n\\[").toList
        .map({ l =>
          l
            .replaceAll(" ", "")
            .replaceAllLiterally("]", "")
            .replaceAllLiterally("[", "")
            .split(",")
            .grouped(2).toList
            .map(p => (p(0).toInt, p(1).toInt))
        })
    } else {
      val dir = new File("../modular-data/code/Modular_Data/" + order)
      import scala.sys.process._
      val pairs = Process("find .", dir).!!.split("\n").collect({
        case s if s.endsWith(".txt") && !s.endsWith("Orbits.txt") && !s.endsWith("3-cocycles.txt") && !s.endsWith("timing.txt") => s.split("\\.")(1).split("/").takeRight(2).map(_.stripSuffix(".txt").toInt)
      }).toList.map(p => (p(0), p(1))).sorted
//      println(pairs)
//      ???
      List(pairs)
    }
  }

  def processOrder(order: Int) {
    val chunks: Seq[Seq[(Int, Int)]] = parseOrbits_txt(order)
    
//    for(chunk <- chunks.par; c <- chunk.par) parseFile(file(order, c))
    
    val classes = (for (chunk <- chunks; c <- findIsomorphismClases(order, chunk)) yield c).seq
    val pw = new PrintWriter(new FileOutputStream(new File("../modular-data/code/Modular_Data/" + order + "/Orbits.txt")))
    for (c <- classes) pw.println(c.map(p => s"[ ${p._1}, ${p._2} ]").mkString("[ ", ", ", " ],").stripSuffix(","))
    pw.close
  }

  //  println(isomorphism(new File("../modular-data/code/Modular_Data/32/6/6.txt"), new File("../modular-data/code/Modular_Data/32/27/10.txt")))

  if (args.isEmpty) {
    for (i <- 48 to 48) {
      println("working on order " + i)
      processOrder(i)
    }
  } else if (args.size == 1) {
    processOrder(args(0).toInt)
  } else {
    val file1 = new File(args(0))
    val file2 = new File(args(1))

    isomorphism(file1, file2) match {
      case Some(i) => println("found isomorphism: " + i.mkString(" "))
      case None    => println("not isomorphic")
    }
  }
}