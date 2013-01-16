package net.tqft.toolkit.algebra.enumeration

import net.tqft.toolkit.algebra.Group
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.Logging

object CanonicalGeneration extends Logging
import CanonicalGeneration.{ info }

// this line is a bit opaque... it says:
//   A represents an eventual concrete realization of this trait, e.g. a TriangleFreeGraph
//   G represents elements of the automorphism group
trait CanonicalGeneration[A <: CanonicalGeneration[A, G], G] { this: A =>
  val automorphisms: FinitelyGeneratedFiniteGroup[G]

  // in each problem instance, we will specify what the upper and lower objects actually look like
  type Lower <: {
    def result: A
    //    def invariant: B
  }

  val ordering: Ordering[Lower]

  type Upper <: {
    val result: A
    def inverse: result.Lower
  }

  // and generate them, along with an action of automorphisms
  def upperObjects: automorphisms.Action[Upper]
  def lowerObjects: automorphisms.Action[Lower]

  // now the actual algorithm
  def children = {
    info("computing children of " + this)
//    info(" automorphism group: " + automorphisms.generators)
    val orbits = upperObjects.orbits.toSeq
//    info(" found " + orbits.size + " orbits, with sizes " + orbits.toSeq.map(_.size).mkString("(", ", ", ")"))
    val result = orbits.flatMap({ orbit =>
      val candidateUpperObject = orbit.representative;
//      info("  considering representative " + candidateUpperObject + " from orbit " + orbit.elements + " with result " + candidateUpperObject.result + " and inverse reduction " + candidateUpperObject.inverse)
      val lowerOrbits = candidateUpperObject.result.lowerObjects.orbits
//      info("  found " + lowerOrbits.size + " lower orbits, with sizes " + lowerOrbits.toSeq.map(_.size).mkString("(", ", ", ")"))
//      info("   which sort as " + lowerOrbits.toSeq.sortBy({ _.representative })(candidateUpperObject.result.ordering).map(_.elements))
      val canonicalReductionOrbit = lowerOrbits.minBy({ _.representative })(candidateUpperObject.result.ordering)
//      info("  canonicalReductionOrbit is " + canonicalReductionOrbit.elements)
      if (canonicalReductionOrbit.contains(candidateUpperObject.inverse)) {
//        info("  which contained the inverse reduction, so we're accepting " + candidateUpperObject.result)
        Some(candidateUpperObject.result)
      } else {
//        info("  which did not contain the inverse reduction, so we're rejecting " + candidateUpperObject.result)
        None
      }
    })
        info("finished computing children of " + this + ", found: " + result.mkString("(", ", ", ")"))
    result
  }

  // and, for convenience, something to recursively find all children, filtering on a predicate
  def descendants(accept: A => Int = { _ => 1 }): Iterator[A] = descendantsTree(accept).map(_._1)
  
  def descendantsTree(accept: A => Int = { _ => 1}): Iterator[(A, Seq[A])] = {
    accept(this) match {
      case a if a > 0 => {
        val c = children
        Iterator((this, c)) ++ c.par.map(_.descendantsTree(accept)).seq.iterator.flatMap(i => i)
      }
      case 0 => Iterator((this, Nil))
      case a if a < 0 => Iterator.empty
    }
  }
  
  def descendantsWithProgress(accept: A => Int = { _ => 1}): Iterator[(A, Seq[(Int, Int)])] = {
    val progress = scala.collection.mutable.Map[A, Seq[(Int, Int)]](this -> Seq((1, 1)))
    descendantsTree(accept).map({
      case (a, children) => {
        for((c, i) <- children.zipWithIndex) {
          progress.put(c, progress(a) :+ (i+1, children.size))
        }
        (a, progress(a))
      }
    })
  }
}

