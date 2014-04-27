package net.tqft.toolkit.algebra.graphs

import scala.sys.process._
import net.tqft.toolkit.Logging
import java.io.InputStream
import java.io.OutputStream
import scala.io.Source
import java.io.PrintWriter
import java.io.File
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups

trait Dreadnaut extends Logging {
  def dreadnautPath: String

  private var in: PrintWriter = null
  private var out: Iterator[String] = null
  private var err: Iterator[String] = null

  protected lazy val initializeDreadnaut = {
    dreadnautPath.run(new ProcessIO(os => in = new PrintWriter(os), is => out = Source.fromInputStream(is).getLines, is => err = Source.fromInputStream(is).getLines))
    while (in == null || out == null) {
      Thread.sleep(10)
    }
    out = out.filterNot(line => line.startsWith("Mode=") || line.startsWith("linelen="))
  }

  // TODO make it possible to access dreadnaut in parallel?
  protected def invokeDreadnaut(cmd: String): Seq[String] = {
    synchronized {
      initializeDreadnaut

      in.println(cmd)
      in.println("\"done... \"z")
      for (i <- 0 until 137) in.println("?") // hideous hack, because somewhere along the way dreadnaut's output is being buffered
      in.flush()
      val result = out.takeWhile(!_.startsWith("done... [")).toList
      result
    }
  }

  def automorphismGroupAndOrbits(g: Graph): (FinitelyGeneratedFiniteGroup[IndexedSeq[Int]], Seq[Seq[Int]]) = {
    val output = invokeDreadnaut(g.toDreadnautString + "cxo\n")
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
    val automorphismGroup = FiniteGroups.symmetricGroup(g.numberOfVertices).subgroupGeneratedBy(generators)
    val orbits = output.last.split(";").toSeq.map(_.trim).filter(_.nonEmpty).map({ orbitString =>
      orbitString.split(" ").toSeq.map(_.trim).filter(_.nonEmpty).filter(!_.startsWith("(")).flatMap({ s =>
        if (s.contains(":")) {
          val a = s.split(":").map(_.toInt)
          a(0) to a(1)
        } else {
          Seq(s.toInt)
        }
      })
    })
    (automorphismGroup, orbits)
  }
  def automorphismGroup(g: Graph) = automorphismGroupAndOrbits(g)._1

  def canonicalLabelling(g: Graph): IndexedSeq[Int] = {
    val output = invokeDreadnaut(g.toDreadnautString + "cxb\n")
    val result = output.dropWhile(!_.startsWith("canupdates")).tail.takeWhile(!_.startsWith("  0 :")).mkString("").split(' ').filter(_.nonEmpty).map(_.toInt)
    require(result.length == g.numberOfVertices)
    result
  }
  def canonicalize(g: Graph): Graph = {
    g.relabel(canonicalLabelling(g))
  }
  def canonicalizeColouredGraph[W](g: ColouredGraph[W]): ColouredGraph[W] = {
    g.relabel(canonicalLabelling(g))
  }
}

object Dreadnaut extends Dreadnaut {
  override val dreadnautPath = try {
    "which dreadnaut".!!
  } catch {
    case e: Exception => System.getProperty("user.home") + "/bin/dreadnaut"
  }

  require(dreadnautPath.nonEmpty, "There doesn't appear to be a copy of dreadnaut on the $PATH.")
  require(invokeDreadnaut("n=3 g 1;2;0; cx").head.startsWith("(1 2)"), "The copy of dreadnaut at " + dreadnautPath + " doesn't seem to be working.")

  initializeDreadnaut
}