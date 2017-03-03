package net.tqft.toolkit.algebra.grouptheory

import scala.collection.GenSeq
import scala.collection.GenSet
import net.tqft.toolkit.algebra.Finite

trait FinitelyGeneratedFiniteGroup[A] extends FiniteGroup[A] { fgFiniteGroup =>
  def generators: Seq[A]
  override def unsortedConjugacyClasses = fgFiniteGroup.conjugationAction.orbits(elements).toSeq

  override def conjugationAction = new super.ConjugationAction with Action[A]

  lazy val elementsAsWordsInGenerators = {
    @scala.annotation.tailrec
    def extendElements(generators: Seq[A], elementsSoFar: Map[A, Seq[Int]], newestElements: Map[A, Seq[Int]]): Map[A, Seq[Int]] = {
      //      FiniteGroup.info("... found " + elementsSoFar.size + " elements of the group so far.")
      if (newestElements.isEmpty) {
        elementsSoFar
      } else {
        val allElements = elementsSoFar ++ newestElements;
        extendElements(generators, allElements, (for ((b, w) <- newestElements; (a, i) <- generators.zipWithIndex) yield (multiply(a, b) -> (i +: w))).toMap -- allElements.keysIterator)
      }
    }

    extendElements(generators, Map.empty, Map(one -> Seq.empty))
  }
  override def elements = elementsAsWordsInGenerators.keySet

  trait Action[B] extends super.Action[B] { action =>
    override def orbits(set: Set[B]) = bruteForceOrbits(generators.toSet, set)
  }

  trait ActionOnFiniteSet[B] extends Action[B] with Finite[B] {
    def allOrbits = orbits(elements.toSet)
  }
}

private case class TrivialFinitelyGeneratedFiniteGroup[A](one: A) extends FinitelyGeneratedFiniteGroup[A] {
  def generators = Seq()
  def inverse(x: A) = one
  def multiply(x: A, y: A) = one
}

object FinitelyGeneratedFiniteGroup {
  def trivialGroup[A](one: A): FinitelyGeneratedFiniteGroup[A] = TrivialFinitelyGeneratedFiniteGroup(one)
}


