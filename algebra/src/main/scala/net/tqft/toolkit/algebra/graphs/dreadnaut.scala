package net.tqft.toolkit.algebra.graphs
import scala.sys.process._
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.Logging

trait dreadnaut extends Logging {
  val dreadnautPath: String

  private def invoke(cmd: String) = {
//    info("invoking dreadnaut with: " + cmd)
    val result = ("echo " + cmd).#|(dreadnautPath).lines
//    debug("dreadnaut output: \n" + result.mkString("\n"))
    result
  }
  
  def automorphismGroup(g: Graph): FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = {
    val output = invoke(g.toDreadnautString + "cx\n")
    val generatorsString = output.filter(_.startsWith("("))
    def permutationFromCycles(cycles: Array[Array[Int]]): IndexedSeq[Int] = {
      for(i <- 0 until g.numberOfVertices) yield {
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
}