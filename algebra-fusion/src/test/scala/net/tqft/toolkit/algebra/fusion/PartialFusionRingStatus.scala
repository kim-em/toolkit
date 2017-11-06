package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.Profiler
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.mutable.ListBuffer
import java.io.File
import scala.io.Source
import net.tqft.toolkit.algebra.enumeration.TreeReader
import java.io.PrintWriter
import java.io.FileOutputStream
import net.tqft.toolkit.algebra.enumeration.TreePrinter
import scala.io.StdIn
import scala.concurrent.Future

object PartialFusionRingStatus extends App {

    import net.tqft.toolkit.collections.Iterators._
    val targets = TreeReader
      .readLeaves(new File("fusion-rings"))

    val minima = scala.collection.mutable.Map[(Int, Int), Double]()
    
    import net.tqft.toolkit.Extractors._
    for(Seq(p, _, _, Double(d)) <- targets.map(_.split(" ").toSeq)) {
      val Seq(Int(sd), Int(dp)) = p.split(",").toSeq
      minima.get((sd,dp)) match {
        case Some(x) if x < d => { }
        case _ => minima((sd,dp)) = d
      }
    }
    for(((sd, dp) , d) <- minima) {
      println(s"$sd,$dp $d")
    }
      
}