package net.tqft.toolkit.algebra.enumeration

import net.tqft.toolkit.algebra.Group
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup

// this line is a bit opaque... it says:
//   A represents an eventual concrete realization of this trait, e.g. a TriangleFreeGraph
//   G represents elements of the automorphism group
//   B represents a value of a complete invariant (possible lazily evaluating)
trait CanonicalGeneration[A <: CanonicalGeneration[A, G, B], G, B] { this: A =>
  val automorphisms: FinitelyGeneratedFiniteGroup[G]

  implicit val ordering: Ordering[B]
  def invariant: B

  // in each problem instance, we will specify what the upper and lower objects actually look like
  type Lower <: {
    val result: A
  }
  type Upper <: {
    val result: A
    def inverse: result.Lower
  }

  // and generate them, along with an action of automorphisms
  def upperObjects: automorphisms.Action[Upper]
  def lowerObjects: automorphisms.Action[Lower]

  // now the actual algorithm
  def children = {
    upperObjects.orbits.flatMap({ orbit =>
      val candidateUpperObject = orbit.representative;
      val canonicalReductionOrbit = candidateUpperObject.result.lowerObjects.orbits.minBy({ _.representative.result.invariant });
      if(canonicalReductionOrbit.contains(candidateUpperObject.inverse)) {
        Some(candidateUpperObject.result)
      } else {
        None
      }
    })

//    for (
//      orbit <- upperObjects.orbits;
//      candidateUpperObject = orbit.representative;
//      canonicalReductionOrbit = candidateUpperObject.result.lowerObjects.orbits.minBy({ _.representative.result.invariant });
//      if canonicalReductionOrbit.contains(candidateUpperObject.inverse)
//    ) yield candidateUpperObject.result
  }

  // and, for convenience, something to recursively find all children, filtering on a predicate
  def descendants(accept: A => Boolean = { _ => true }): Iterator[A] = {
    if (accept(this)) {
      Iterator(this) ++ children.toIterator.flatMap(_.descendants(accept))
    } else {
      Iterator.empty
    }
  }
}

