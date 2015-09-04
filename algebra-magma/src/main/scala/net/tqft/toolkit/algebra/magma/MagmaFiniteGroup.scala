package net.tqft.toolkit.algebra.magma

import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import java.io.PrintWriter
import scala.io.Source
import scala.sys.process._
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.permutations.Permutations.Permutation
import net.tqft.toolkit.algebra.Finite
import net.tqft.toolkit.algebra.grouptheory.FiniteGroupHomomorphism
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroupHomomorphism

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
      in.println("SetColumns(0);")
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

  def numberOfSmallGroups(order: Int) = invokeMagma(s"NumberOfSmallGroups($order);").head.toInt

  def smallGroup(order: Int, index: Int): FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = {
    MagmaGroup(s"SmallGroup($order, $index)").asScalaGroup
  }

  trait MagmaGroupLoader { G =>
    def magmaConstructor: String
    lazy val asScalaGroup: FinitelyGeneratedFiniteGroup[IndexedSeq[Int]] = {
      val output = invokeMagma(s"G:=$magmaConstructor; CosetTable(G,sub<G|>);")
      require(output.head.startsWith("Mapping from:"), "Magma error while building group: " + magmaConstructor)
      val generators = try {
        output.drop(2).takeWhile(_.trim.nonEmpty).map(_.trim.split("\\s+").toSeq.tail.map(_.toInt - 1)).transpose.map(_.toIndexedSeq.asInstanceOf[Permutation])
      } catch {
        case e: NumberFormatException => {
          println("NumberFormatException while parsing:")
          println(output.mkString("\n"))
          ???
        }
      }
      FiniteGroups.symmetricGroup(generators.head.size).subgroupGeneratedBy(generators)
    }
    def numberOfSubgroups(indexBound: Int = G.asScalaGroup.size) = invokeMagma(s"#LowIndexSubgroups($magmaConstructor, $indexBound);").head.toInt

    case class MagmaSubgroup(index: Int, indexBound: Int = G.asScalaGroup.size) extends MagmaGroupLoader { sbgrp =>
      override def magmaConstructor = s"LowIndexSubgroups(${G.magmaConstructor}, $indexBound)[$index]"
      lazy val cosetAction: G.asScalaGroup.ActionOnFiniteSet[Int] = {
        val output = invokeMagma(s"G:=${G.magmaConstructor}; CosetTable(G,LowIndexSubgroups(G, $indexBound)[$index]);")
        require(output.head.startsWith("Mapping from:"))
        val actionOfGenerators = output.drop(2).takeWhile(_.trim.nonEmpty).map(_.trim.split("\\s+").toSeq.tail.map(_.toInt - 1)).transpose.map(_.toIndexedSeq.asInstanceOf[Permutation])
        new G.asScalaGroup.ActionOnFiniteSet[Int] {
          override def elements = actionOfGenerators.head.toSet
          override def act(a: IndexedSeq[Int], b: Int) = {
            asScalaGroup.generators.indexOf(a) match {
              case -1 => {
                val word = G.asScalaGroup.elementsAsWordsInGenerators(a)
                word.foldRight(b)({ case (k, c) => actionOfGenerators(k)(c) })
              }
              case k => actionOfGenerators(k)(b)
            }
          }
        }
      }
    }
    def actions = for (i <- 1 to numberOfSubgroups()) yield MagmaSubgroup(i).cosetAction

    case object Square extends MagmaGroupLoader { GxG =>
      override val magmaConstructor = s"DirectProduct(${G.magmaConstructor}, ${G.magmaConstructor})"
      private trait Inclusion extends FinitelyGeneratedFiniteGroupHomomorphism[IndexedSeq[Int], IndexedSeq[Int]] {
        override val source = G.asScalaGroup
        override val target = GxG.asScalaGroup
      }
      val leftInclusion: FinitelyGeneratedFiniteGroupHomomorphism[IndexedSeq[Int], IndexedSeq[Int]] = new Inclusion {
        def applyToGenerator(i: Int) = {
          target.generators(i)
        }
      }
      val rightInclusion: FinitelyGeneratedFiniteGroupHomomorphism[IndexedSeq[Int], IndexedSeq[Int]] = new Inclusion {
        def applyToGenerator(i: Int) = {
          target.generators(i + source.generators.size)
        }
      }
      
      case class Action(i: Int, indexBound: Int = G.asScalaGroup.size * G.asScalaGroup.size) {
        lazy val asScalaAction = GxG.MagmaSubgroup(i, indexBound).cosetAction
        private lazy val X = 0 // asScalaAction.elements.head
        val leftAction = new G.asScalaGroup.Action[Int] {
          override def act(g: IndexedSeq[Int], b: Int) = asScalaAction.act(leftInclusion(g), b)
        }
        val rightAction = new G.asScalaGroup.Action[Int] {
          override def act(g: IndexedSeq[Int], b: Int) = asScalaAction.act(rightInclusion(g), b)
        }
        lazy val leftActionOnRightCosets = new G.asScalaGroup.Action[rightAction.Orbit] {
          //          val elements = rightAction.orbits(asScalaAction.elements.toSet)
          override def act(g: IndexedSeq[Int], b: rightAction.Orbit) = rightAction.orbits(asScalaAction.elements.toSet).find(orbit => orbit.contains(asScalaAction.act(leftInclusion(g), b.representative))).get
        }
        lazy val rightActionOnLeftCosets = new G.asScalaGroup.Action[leftAction.Orbit] {
          //          val elements = leftAction.orbits(asScalaAction.elements.toSet)
          override def act(g: IndexedSeq[Int], b: leftAction.Orbit) = leftAction.orbits(asScalaAction.elements.toSet).find(orbit => orbit.contains(asScalaAction.act(rightInclusion(g), b.representative))).get
        }
        lazy val leftStabilizer: G.asScalaGroup.Subgroup = {
          G.asScalaGroup.stabilizer(leftAction, X)
        }
        lazy val rightStabilizer: G.asScalaGroup.Subgroup = {
          G.asScalaGroup.stabilizer(rightAction, X)
        }
        lazy val L: G.asScalaGroup.Subgroup = {
          G.asScalaGroup.stabilizer(leftActionOnRightCosets, rightAction.orbits(asScalaAction.elements.toSet).find(orbit => orbit.contains(X)).get)
        }
        lazy val R: G.asScalaGroup.Subgroup = {
          G.asScalaGroup.stabilizer(rightActionOnLeftCosets, leftAction.orbits(asScalaAction.elements.toSet).find(orbit => orbit.contains(X)).get)
        }
        lazy val doubleCosets = FiniteGroups.doubleCosets(G.asScalaGroup, L, R)
      }
    }
  }

  case class MagmaGroup(override val magmaConstructor: String) extends MagmaGroupLoader { G =>

  }

  def smallGroupWithActions(order: Int, index: Int): (FinitelyGeneratedFiniteGroup[IndexedSeq[Int]], IndexedSeq[FinitelyGeneratedFiniteGroup[IndexedSeq[Int]]#ActionOnFiniteSet[Int]]) = {
    val magmaGroup = MagmaGroup(s"SmallGroup($order, $index)")
    (magmaGroup.asScalaGroup, magmaGroup.actions)
  }
}

object Magma extends Magma {
  override val magmaCommand = "/Users/scott/bin/orac /usr/local/bin/magma"
}