package net.tqft.toolkit.algebra.grouptheory

import net.tqft.toolkit.algebra._
import scala.collection.GenSet

// TODO eliminate this in favour of the inner version

trait GroupAction[A, B] {
  def act(a: A, b: B): B
  def orbits(generators: Set[A], objects: Set[B]): Set[Orbit[A, B]] = {
    class O(val representative: B) extends Orbit[A, B] {
      override def stabilizer = ???
      override lazy val elements = extendElements(scala.collection.immutable.HashSet.empty[B], Set(representative))

      @scala.annotation.tailrec
      private def extendElements(elements: Set[B], newestElements: GenSet[B]): Set[B] = {
        if (newestElements.isEmpty) {
          elements
        } else {
          val allElements = elements union newestElements;
          extendElements(allElements, (for (b <- newestElements; a <- generators) yield act(a, b)) diff allElements)
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

    extractOrbits(objects, Set())
  }
}
