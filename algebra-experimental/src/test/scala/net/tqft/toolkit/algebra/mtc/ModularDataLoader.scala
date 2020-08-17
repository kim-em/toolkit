package net.tqft.toolkit.algebra.mtc

import net.tqft.toolkit.functions.Memo
import java.io.File
import scala.io.Source

object ModularDataLoader {
  var stringCounter = 0
  val stringHashMap = scala.collection.parallel.mutable.ParHashMap[String, Int]()
  def stringHash(s: String) : Int = {
    stringHashMap.synchronized {
      stringHashMap.get(s) match {
        case Some(i) => i
        case None => {
          val r = stringCounter
          stringHashMap += ((s, r))
          stringCounter = stringCounter + 1
          r
        }
      }
    }
  }

  // returns S and T matrices, read from a file in Angus' GAP output format.
  def parseFile_(file: File): (IndexedSeq[IndexedSeq[Int]], IndexedSeq[Int]) = {
//    print("parsing " + file)
    val source = Source.fromFile(file)
    val lines = source.getLines.toList
    val length = lines.length
    def parseMatrix(s: String) = s.replaceAllLiterally("[", "").replaceAllLiterally("]", "").replaceAllLiterally(" ", "").split(",")
    val T = parseMatrix(lines(length - 2).drop(5).dropRight(1)).toIndexedSeq.map(stringHash)
    val S = parseMatrix(lines(length - 3).drop(5).dropRight(1)).grouped(T.size).toIndexedSeq.map(_.toIndexedSeq.map(stringHash))
//    println(" ... finished")
    (S, T)
  }

  //  val parseFile = Memo(parseFile_ _)
  val parseFile = Memo.softly(parseFile_ _)
  //  val parseFile = parseFile_ _

  def remap(ms: Seq[(IndexedSeq[IndexedSeq[Int]], IndexedSeq[Int])]): Seq[(IndexedSeq[IndexedSeq[Int]], IndexedSeq[Int])] = {
    val S_values = ms.head._1.flatten.sorted.distinct
    val T_values = ms.head._2.sorted.distinct

    //    for (m <- ms.tail) {
    //      require(m._1.flatten.sorted.distinct == S_values, ms)
    //      require(m._2.sorted.distinct == T_values)
    //    }

    val S_index = S_values.zipWithIndex.toMap
    val T_index = T_values.zipWithIndex.toMap

    for (m <- ms) yield {
      val S = m._1.map(r => r.map(s => S_index(s)))
      val T = m._2.map(x => T_index(x))
      (S, T)
    }
  }

  def superimposedST(file: File): IndexedSeq[IndexedSeq[(Int, Option[Int])]] = {
    val p = ModularDataLoader.parseFile(file)
    val Seq(m) = remap(Seq(p))
    val rank = m._2.length
    (for (i <- 0 until rank) yield {
      (for (j <- 0 until rank) yield {
        (p._1(i)(j), if (i == j) Some(p._2(i)) else None)
      })
    })
  }

}