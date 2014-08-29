package net.tqft.toolkit.algebra.enumeration

import net.tqft.toolkit.algebra.Group
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.Logging

import scala.language.reflectiveCalls // ouch...

object CanonicalGeneration extends Logging
import CanonicalGeneration.{ info }

// this line is a bit opaque... it says:
//   A represents an eventual concrete realization of this trait, e.g. a TriangleFreeGraph
//   G represents elements of the automorphism group
trait CanonicalGeneration[A <: CanonicalGeneration[A, G], G] { this: A =>
  val automorphisms: FinitelyGeneratedFiniteGroup[G]
  def findIsomorphismTo(other: A): Option[G]

  // in each problem instance, we will specify what the upper and lower objects actually look like
  type Lower <: {
    def result: A
  }

  // BE CAREFUL: this ordering must be invariant under the automorphism action on Lowers
  def ordering: Ordering[lowerObjects.Orbit]

  type Upper <: {
    val result: A
    def inverse: result.Lower
  }

  // and generate them, along with an action of automorphisms
  def upperObjects: automorphisms.Action[Upper]
  val lowerObjects: automorphisms.Action[Lower]

  // now the actual algorithm
  def children = {
//    info("computing children of " + this)
//    info(" automorphism group: " + automorphisms.generators)
    val orbits = upperObjects.orbits.toSeq
//    info(" found " + orbits.size + " orbits, with sizes " + orbits.toSeq.map(_.size).mkString("(", ", ", ")"))
    val result = orbits.flatMap({ orbit =>
      val candidateUpperObject = orbit.representative;
//      info("  considering representative " + candidateUpperObject + " from orbit " + orbit.elements)
//      info("   with result " + candidateUpperObject.result + " and inverse reduction " + candidateUpperObject.inverse)
      val lowerOrbits = candidateUpperObject.result.lowerObjects.orbits
//      info("  found " + lowerOrbits.size + " lower orbits, with sizes " + lowerOrbits.toSeq.map(_.size).mkString("(", ", ", ")"))
//      info("   which sort as " + lowerOrbits.toSeq.sorted(candidateUpperObject.result.ordering).map(_.elements))
      val canonicalReductionOrbit = lowerOrbits.min(candidateUpperObject.result.ordering)
//      info("  canonicalReductionOrbit is " + canonicalReductionOrbit.elements)
      if (canonicalReductionOrbit.contains(candidateUpperObject.inverse)) {
//        info("  which contained the inverse reduction, so we're accepting " + candidateUpperObject.result)
        Some(candidateUpperObject.result)
      } else {
//        info("  which did not contain the inverse reduction, so we're rejecting " + candidateUpperObject.result)
        None
      }
    })
//    info("finished computing children of " + this + ", found: " + result.mkString("(", ", ", ")"))
    result
  }

  def isomorphicTo_?(other: A) = findIsomorphismTo(other).nonEmpty

  def parent = {
    val elts = lowerObjects.orbits
    if (elts.isEmpty) {
      None
    } else {
      Some(elts.min(ordering).representative.result)
    }
  }

  def ancestry = Iterator.iterate[Option[A]](Some(this))(o => o.flatMap(_.parent)).takeWhile(_.nonEmpty).map(_.get)
  def progenitor = {
    import net.tqft.toolkit.collections.Iterators._
    ancestry.last
  }

  def verifyParent = {
    parent.map(_.children.exists(isomorphicTo_?)).getOrElse(true)
  }
  def verifyAncestry = ancestry.forall(_.verifyParent)

  def isomorphs: Iterator[A]
  def verifyAncestryForSomeIsomorph = isomorphs.exists(_.verifyAncestry)

  def verifyUpperOrbits = {
    (for (o <- upperObjects.orbits.iterator; s <- o.elements.subsets(2); Seq(a, b) = s.toSeq) yield {
      a.result.isomorphicTo_?(b.result)
    }) ++
      (for (s <- upperObjects.orbits.subsets(2); Seq(o1, o2) = s.toSeq; u1 = o1.representative; u2 = o2.representative) yield {
        !u1.result.isomorphicTo_?(u2.result)
      })
  }

  def verifyInverses = {
    val badPairOption = upperObjects.elements.map(u => (u, u.result.lowerObjects.elements.find(_.result != this))).find(_._2.nonEmpty).map(p => (p._1, p._2.get))
    badPairOption.map({
      case (u, l) => println(s"Upper object $u for $this has bad inverse $l")
    })
    badPairOption.isEmpty
  }

  // and, for convenience, something to recursively find all children, filtering on a predicate
  def descendants(accept: A => Int = { _ => 1 }): Iterator[A] = descendantsTree(accept).map(_._1)

  def descendantsTree(accept: A => Int = { _ => 1 }): Iterator[(A, Seq[A])] = {
    accept(this) match {
      case a if a > 0 => {
        val c = children
        //        lazy val d1 = c.par.map(_.descendantsTree(accept)).seq.iterator.flatMap(i => i) // this seems to make mathematica sick?
        lazy val d2 = c.iterator.map(_.descendantsTree(accept)).flatten
        Iterator((this, c)) ++ d2
      }
      case 0 => Iterator((this, Nil))
      case a if a < 0 => Iterator.empty
    }
  }

  def descendantsTreeAvoiding(instances: Seq[A], accept: A => Int = { _ => 1 }) = descendantsTree({ a: A =>
    if (instances.exists(_.isomorphicTo_?(a))) {
      0
    } else {
      accept(a)
    }
  })

  def descendantsWithProgress(accept: A => Int = { _ => 1 }): Iterator[(A, Seq[(Int, Int)])] = {
    // TODO don't save progress forever
    val progress = scala.collection.mutable.Map[Int, Seq[(Int, Int)]](this.hashCode -> Seq((1, 1)))
    descendantsTree(accept).map({
      case (a, children) => {
        for ((c, i) <- children.zipWithIndex) {
          progress.put(c.hashCode, progress(a.hashCode) :+ (i + 1, children.size))
        }
        (a, progress(a.hashCode))
      }
    })
  }
  def randomLineage: Iterator[(A, Seq[A], Long)] = {
    import net.tqft.toolkit.collections.Iterators._
    val (t, nc) = net.tqft.toolkit.Profiler.timing(this.children)
    Iterator.iterateUntilNone((this, nc, t))({
      case (_, c, _) if c.nonEmpty => {
        val n = c(scala.util.Random.nextInt(c.size))
        val (t, nc) = net.tqft.toolkit.Profiler.timing(n.children)
        Some((n, nc, t))
      }
      case _ => None
    })
  }
  def numberOfDescendantsEstimators: Iterator[Int] = {
    def estimate = randomLineage.map(_._2.size).filterNot(_ == 0).product
    def estimates = Iterator.continually(estimate)
    def partialSums = estimates.scanLeft(0)(_ + _)
    def averages = partialSums.zipWithIndex.collect({ case (s, i) if i != 0 => s / i })
    averages
  }
  def runtimeEstimators: Iterator[Long] = {
    def estimate = randomLineage.foldLeft((1, 0L))({ (a, b) => (a._1 * b._2.size, a._2 + a._1 * b._3) })._2
    def estimates = Iterator.continually(estimate)
    def partialSums = estimates.scanLeft(0L)(_ + _)
    def averages = partialSums.zipWithIndex.collect({ case (s, i) if i != 0 => s / i })
    averages
  }

}

