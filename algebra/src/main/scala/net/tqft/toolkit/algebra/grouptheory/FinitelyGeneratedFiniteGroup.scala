package net.tqft.toolkit.algebra.grouptheory

import scala.collection.GenSeq

trait FinitelyGeneratedFiniteGroup[A] extends FiniteGroup[A] { fgFiniteGroup =>
  def generators: Set[A]
  override def unsortedConjugacyClasses = GroupActions.conjugationAction(fgFiniteGroup).orbits(generators, elements).toSeq

  trait Action[B] extends super.Action[B] { action =>
    def elements: Set[B]
    def orbits: Set[Orbit[A, B]] = {
//      val orbitMap = scala.collection.mutable.Map[B, Set[B]]()

      
      
      class O(val representative: B) extends Orbit[A, B] {
        override def stabilizer = ???
        override lazy val elements = extendElements(Seq.empty, Seq(representative).par).toSet

        @scala.annotation.tailrec
        private def extendElements(elements: Seq[B], newestElements: GenSeq[B]): Seq[B] = {
          if (newestElements.isEmpty) {
            elements
          } else {
            val allElements = elements ++ newestElements;
            extendElements(allElements, (for (b <- newestElements; a <- generators) yield act(a, b)).distinct.filterNot(allElements.contains))
          }
        }
      }

      def extractOrbits(objects: Set[B], orbits: Set[Orbit[A, B]]): Set[Orbit[A, B]] = {
        if (objects.isEmpty) {
          orbits
        } else {
          val newOrbit = new O(objects.head)
          extractOrbits(objects diff newOrbit.elements, orbits + newOrbit)
        }
      }

      extractOrbits(action.elements, Set())
    }

  }
}




