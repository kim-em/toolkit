package net.tqft.toolkit.algebra.graphs
import scala.sys.process._
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.Logging
import java.io.InputStream
import java.io.OutputStream
import scala.io.Source
import java.io.PrintWriter

trait dreadnaut extends Logging {
  val dreadnautPath: String

  private var in: PrintWriter = null
  private var out: Iterator[String] = null

  private lazy val initialize = {
    dreadnautPath.run(new ProcessIO(os => in = new PrintWriter(os), is => out = Source.fromInputStream(is).getLines, _.close()))
    while (in == null || out == null) {
      Thread.sleep(10)
    }
    out = out.filterNot(line => line.startsWith("Mode=") || line.startsWith("linelen="))
  }

  // TODO make it possible to access dreadnaut in parallel?
  private def invoke(cmd: String): Seq[String] = {
    synchronized {
      initialize

      in.println(cmd)
      in.println("\"done... \"z")
      for (i <- 0 until 137) in.println("?") // hideous hack, because somewhere along the way dreadnaut's output is being buffered
      in.flush()
      val result = out.takeWhile(!_.startsWith("done... [")).toList
      result
    }
  }

  //  private def invoke(cmd: String): Stream[String] = {
  //    ("echo " + cmd).#|(dreadnautPath).lines
  //  }

  def automorphismGroup(g: Graph): FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = {
    val output = invoke(g.toDreadnautString + "cx\n")
    val generatorsString = output.filter(_.startsWith("("))
    def permutationFromCycles(cycles: Array[Array[Int]]): IndexedSeq[Int] = {
      for (i <- 0 until g.numberOfVertices) yield {
        cycles.find(_.contains(i)) match {
          case Some(cycle) => cycle((cycle.indexOf(i) + 1) % cycle.length)
          case None => i
        }
      }
    }
    val generators = generatorsString.map(line => permutationFromCycles(line.split('(').filter(_.nonEmpty).map(_.stripSuffix(")").split(" ").map(_.toInt)))).toSet
    FiniteGroups.symmetricGroup(g.numberOfVertices).subgroupGeneratedBy(generators)
  }
  def canonicalLabelling(g: Graph): IndexedSeq[Int] = {
    val output = invoke(g.toDreadnautString + "cxb\n")
    output.dropWhile(!_.startsWith("canupdates")).tail.takeWhile(!_.startsWith("  0 :")).mkString("").split(' ').filter(_.nonEmpty).map(_.toInt)
  }
  def canonicalize(g: Graph): Graph = {
    g.relabel(canonicalLabelling(g))
  }
}

object dreadnaut extends dreadnaut {
  override val dreadnautPath = "whereis dreadnaut" !!;
  require(dreadnautPath.nonEmpty, "There doesn't appear to be a copy of dreadnaut on the $PATH.")
  require(invoke("n=3 g 1;2;0; cx").head.startsWith("(1 2)"), "The copy of dreadnaut at " + dreadnautPath + " doesn't seem to be working.")

  initialize
}