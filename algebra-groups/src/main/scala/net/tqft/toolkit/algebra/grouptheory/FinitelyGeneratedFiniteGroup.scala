package net.tqft.toolkit.algebra.grouptheory

import scala.collection.GenSeq
import scala.collection.GenSet

trait FinitelyGeneratedFiniteGroup[A] extends FiniteGroup[A] { fgFiniteGroup =>
  def generators: Set[A]
  override def unsortedConjugacyClasses = GroupActions.conjugationAction(fgFiniteGroup).orbits(generators, elements).toSeq

  override lazy val elements = {
    @scala.annotation.tailrec
    def extendElements(generators: Set[A], elementsSoFar: Set[A], newestElements: GenSet[A]): Set[A] = {
      FiniteGroup.info("... found " + elementsSoFar.size + " elements of the group so far.")
      if (newestElements.isEmpty) {
        elementsSoFar
      } else {
        val allElements = elementsSoFar union newestElements;
        extendElements(generators, allElements, (for (b <- newestElements; a <- generators) yield multiply(a, b)) diff allElements)
      }
    }

    extendElements(generators, Set(one), generators)
  }

  trait Action[B] extends super.Action[B] { action =>
    def elements: Seq[B]

    trait Orbit extends net.tqft.toolkit.algebra.grouptheory.Orbit[A, B]

    def orbits: Set[Orbit] = {
      //      for(a <- elements; g <- generators) {
      //        require(elements.contains(act(g, a)))
      //      }

      class O(val representative: B) extends Orbit {
        override def stabilizer = ???
        override lazy val elements = extendElements(Seq.empty, Seq(representative).par).toSet

        override def hashCode = elements.hashCode
        override def equals(other: Any) = {
          other match {
            case other: FinitelyGeneratedFiniteGroup[A]#Action[B]#Orbit => other.elements == elements
            case _ => false
          }
        }

        @scala.annotation.tailrec
        private def extendElements(elements: Seq[B], newestElements: GenSeq[B]): Seq[B] = {
          if (newestElements.isEmpty) {
            elements
          } else {
            val allElements = (elements ++ newestElements).distinct;
            extendElements(allElements, (for (b <- newestElements; a <- generators) yield act(a, b)).distinct.filterNot(allElements.contains))
          }
        }
      }

      @scala.annotation.tailrec
      def extractOrbits(objects: Set[B], orbits: Set[Orbit]): Set[Orbit] = {
        if (objects.isEmpty) {
          orbits
        } else {
          val newOrbit = new O(objects.head)
          extractOrbits(objects diff newOrbit.elements, orbits + newOrbit)
        }
      }

      extractOrbits(action.elements.toSet, Set())
    }

  }
}




