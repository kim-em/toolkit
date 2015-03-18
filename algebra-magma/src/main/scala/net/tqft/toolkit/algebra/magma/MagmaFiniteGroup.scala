package net.tqft.toolkit.algebra.magma

import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import java.io.PrintWriter
import scala.io.Source
import scala.sys.process._
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.permutations.Permutations.Permutation
import net.tqft.toolkit.algebra.Finite

trait Magma {
  def magmaCommand: String

  private case class Pipes(var in: PrintWriter, var out: Iterator[String], var err: Iterator[String])

  private val pipes = new ThreadLocal[Pipes]

  private def in = pipes.get.in
  private def out = pipes.get.out
  private def err = pipes.get.err

  protected def initializeMagma = {
    if (pipes.get == null || in == null) {
      val p = Pipes(null, null, null)
      pipes.set(p)
      magmaCommand.run(new ProcessIO(os => p.in = new PrintWriter(os), is => p.out = Source.fromInputStream(is).getLines.drop(2), is => p.err = Source.fromInputStream(is).getLines))
      while (in == null || out == null) {
        Thread.sleep(10)
      }
//      p.out = p.out.map(x => {println("out: " + x); x})
    }
  }
  def invokeMagma(cmd: String): Seq[String] = {
    initializeMagma

//    println(s"in: $cmd")
    in.println(cmd)
    in.println("\"done...\";")
    in.flush()
    val result = out.takeWhile(_ != "done...").toList
    result
  }

  def smallGroup(order: Int, index: Int): FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = {
    MagmaGroup(s"SmallGroup($order, $index)").group
  }

  trait MagmaGroupLoader {
    def magmaConstructor: String
    lazy val group: FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = {
      val output = invokeMagma(s"G:=$magmaConstructor; CosetTable(G,sub<G|>);")
      require(output.head.startsWith("Mapping from:"))
      val generators = output.drop(2).takeWhile(_.trim.nonEmpty).map(_.trim.split("\\s+").toSeq.tail.map(_.toInt - 1)).transpose.map(_.toIndexedSeq.asInstanceOf[Permutation])
      FiniteGroups.symmetricGroup(generators.head.size).subgroupGeneratedBy(generators)
    }
  }

  case class MagmaGroup(override val magmaConstructor: String) extends MagmaGroupLoader { grp =>
    def numberOfSubgroups = invokeMagma(s"#Subgroups($magmaConstructor);").head.toInt
    case class MagmaSubgroup(index: Int) extends MagmaGroupLoader { sbgrp =>
      override def magmaConstructor = s"Subgroups(${grp.magmaConstructor})[$index]`subgroup"
      lazy val cosetAction: grp.group.ActionOnFiniteSet[Int] = {
        val output = invokeMagma(s"G:=${grp.magmaConstructor}; CosetTable(G,Subgroups(G)[$index]`subgroup);")
        require(output.head.startsWith("Mapping from:"))
        val actionOfGenerators = output.drop(2).takeWhile(_.trim.nonEmpty).map(_.trim.split("\\s+").toSeq.tail.map(_.toInt - 1)).transpose.map(_.toIndexedSeq.asInstanceOf[Permutation])
        new grp.group.ActionOnFiniteSet[Int] {
          override def elements = actionOfGenerators.head.toSet
          override def act(a: IndexedSeq[Int], b: Int) = {
            grp.group.generators.indexOf(a) match {
              case -1 => {
                val word = grp.group.elementsAsWordsInGenerators(a)
                word.foldRight(b)({ case (k, c) => actionOfGenerators(k)(c) })
              }
              case k => actionOfGenerators(k)(b)
            }
          }
        }
      }
    }
  }

  def smallGroupWithActions(order: Int, index: Int): (FinitelyGeneratedFiniteGroup[IndexedSeq[Int]], IndexedSeq[FinitelyGeneratedFiniteGroup[IndexedSeq[Int]]#ActionOnFiniteSet[Int]]) = {
    val magmaGroup = MagmaGroup(s"SmallGroup($order, $index)")
    (magmaGroup.group, for(i <- 1 to magmaGroup.numberOfSubgroups) yield magmaGroup.MagmaSubgroup(i).cosetAction)
  }
}

object Magma extends Magma {
  override val magmaCommand = "/Users/scott/bin/orac /usr/local/bin/magma"
}