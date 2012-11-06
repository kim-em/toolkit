package net.tqft.toolkit.algebra.enumeration

trait EquivalenceClass[X] {
  def representative: X
  def contains(x: X): Boolean
}

trait Group[G] {
  trait Action[X] {
    def orbits: Set[EquivalenceClass[X]]
    def leastOrbit[B: Ordering](invariant: X => B): EquivalenceClass[X]
  }
}

trait CanonicalGeneration[A <: CanonicalGeneration[A, G, B], G, B] {
  val automorphisms: Group[G]

  implicit val ordering: Ordering[B]
  def invariant: B

  trait Upper {
    val result: A
    def inverse: result.Lower
  }
  trait Lower {
    val result: A
  }

  def upperObjects: automorphisms.Action[Upper]
  def lowerObjects: automorphisms.Action[Lower]

// FIXME Not quite there yet!
  //  def children = {
//    for (
//      orbit <- upperObjects.orbits;
//      candidateUpperObject = orbit.representative;
//      canonicalReductionOrbit = candidateUpperObject.result.lowerObjects.leastOrbit({ l: candidateUpperObject.result.Lower => l.result.invariant });
//      if canonicalReductionOrbit.contains(candidateUpperObject.inverse)
//    ) yield candidateUpperObject.result
//  }
}