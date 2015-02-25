package net.tqft.toolkit.algebra.enumeration

import net.tqft.toolkit.algebra.Group
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.Logging
import scala.language.reflectiveCalls
import CanonicalGeneration.{ info }
import scala.collection.immutable.Queue
import net.tqft.toolkit.collections.Iterators

object CanonicalGeneration extends Logging

// this line is a bit opaque... it says:
//   A represents an eventual concrete realization of this trait, e.g. a TriangleFreeGraph
//   G represents elements of the automorphism group
trait CanonicalGeneration[A <: CanonicalGeneration[A, G], G] { this: A =>
  val automorphisms: FinitelyGeneratedFiniteGroup[G]

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
    //        info("computing children of " + this)
    //        info(" automorphism group: " + automorphisms.generators)
    val orbits = upperObjects.orbits.toSeq
    //        info(" found " + orbits.size + " orbits, with sizes " + orbits.toSeq.map(_.size).mkString("(", ", ", ")"))
    val result = orbits.flatMap({ orbit =>
      val candidateUpperObject = orbit.representative;
      //            info("  considering representative " + candidateUpperObject + " from orbit " + orbit.elements)
      //            info("   with result " + candidateUpperObject.result + " and inverse reduction " + candidateUpperObject.inverse)
      val lowerOrbits = candidateUpperObject.result.lowerObjects.orbits
      //            info("  found " + lowerOrbits.size + " lower orbits, with sizes " + lowerOrbits.toSeq.map(_.size).mkString("(", ", ", ")"))
      //            info("   which sort as " + lowerOrbits.toSeq.sorted(candidateUpperObject.result.ordering).map(_.elements))
      val canonicalReductionOrbit = lowerOrbits.min(candidateUpperObject.result.ordering)
      //            info("  canonicalReductionOrbit is " + canonicalReductionOrbit.elements)
      //            info("  with result " + canonicalReductionOrbit.representative.result)
      if (canonicalReductionOrbit.contains(candidateUpperObject.inverse)) {
        //                info("  which contained the inverse reduction, so we're accepting " + candidateUpperObject.result)
        Some(candidateUpperObject.result)
      } else {
        //                info("  which did not contain the inverse reduction, so we're rejecting " + candidateUpperObject.result)
        None
      }
    })
    //        info("finished computing children of " + this + ", found: " + result.mkString("(", ", ", ")"))
    result
  }

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

  def verifyInverses = {
    val badPairOption = upperObjects.elements.map(u => (u, u.result.lowerObjects.elements.find(_.result != this))).find(_._2.nonEmpty).map(p => (p._1, p._2.get))
    badPairOption.map({
      case (u, l) => println(s"Upper object $u for $this has bad inverse $l")
    })
    badPairOption.isEmpty
  }

  private def childrenResMod(c: Seq[A], res: Int, mod: Int): (Seq[A], Int, Int) = {
    if (mod == 1) {
      (c, 0, 1)
    } else if (c.isEmpty) {
      (Nil, 0, 1)
    } else if (c.size < mod) {
      (Seq(c(res % c.size)), res / c.size, (mod + c.size - 1 - (res % c.size)) / c.size)
    } else {
      (for (i <- res until c.size by mod) yield c(i), 0, 1)
    }
  }

  // and, for convenience, something to recursively find all children, filtering on a predicate
  def descendants(accept: A => Double = { _ => 1 }, res: Int = 0, mod: Int = 1): Iterator[A] = {
    require(mod > 0)
    require(res >= 0)
    require(res < mod)

    def thisIterator = if (res == 0) {
      Iterator(this)
    } else {
      Iterator.empty
    }

    accept(this) match {
      case a if a > 0 => {
        thisIterator ++ Iterator.continually(childrenResMod(children, res, mod)).take(1).map({ case (subset, r, m) => subset.iterator.flatMap(_.descendants(accept, r, m)) }).flatten
      }
      case 0 => {
        thisIterator
      }
      case a if a < 0 => Iterator.empty
    }
  }

  
  
  def parDescendants(accept: A => Double = { _ => 1}, mod: Int = Runtime.getRuntime().availableProcessors() * 3): Iterator[A] = {
    Iterators.parallelCombine(for(res <- 0 until mod) yield descendants(accept, res, mod))
  }
  
  def descendantsTree(accept: A => Double = { _ => 1 }, res: Int = 0, mod: Int = 1): Iterator[(A, Seq[A])] = {
    def thisIterator(c: Seq[A]) = if (res == 0) {
      Iterator((this, c))
    } else {
      Iterator.empty
    }

    accept(this) match {
      case a if a > 0 => {
        val c = children
        val (subset, r, m) = childrenResMod(c, res, mod)
        thisIterator(c) ++ subset.iterator.flatMap(_.descendantsTree(accept, r, m))
      }
      case 0 => thisIterator(Nil)
      case a if a < 0 => Iterator.empty
    }
  }
  
  def descendantsWithProgress(accept: A => Double = { _ => 1 }, res: Int = 0, mod: Int = 1): Iterator[(A, Seq[(Int, Int)])] = {
    // TODO don't save progress forever
    val progress = scala.collection.mutable.Map[Int, Seq[(Int, Int)]](this.hashCode -> Seq((1, 1)))
    descendantsTree(accept, res, mod).map({
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
  private def verboseDuration(x: Long): String = {
    val sb = new StringBuffer
    val second = 1000L
    val minute = 60 * second
    val hour = 60 * minute
    val day = 24 * hour
    val year = 365 * day
    if (x > year) {
      sb.append((x / year) + " years ")
    }
    if (x > day) {
      sb.append((x % year) / day + " days ")
    }
    if (x > hour) {
      sb.append((x % day) / hour + " hours ")
    }
    if (x > minute) {
      sb.append((x % hour) / minute + " minutes ")
    }
    sb.append((x % minute) / (second.toDouble) + " seconds.")

    sb.toString
  }
  def runtimeEstimatorStrings = runtimeEstimators.map(verboseDuration)

}

trait CanonicalGenerationWithIsomorphism[A <: CanonicalGenerationWithIsomorphism[A, G], G] extends CanonicalGeneration[A, G] { this: A =>
  def findIsomorphismTo(other: A): Option[G]
  def isomorphs: Iterator[A]
  def isomorphicTo_?(other: A) = findIsomorphismTo(other).nonEmpty

  def descendantsTreeAvoiding(instances: Seq[A], accept: A => Int = { _ => 1 }) = descendantsTree({ a: A =>
    if (instances.exists(_.isomorphicTo_?(a))) {
      0
    } else {
      accept(a)
    }
  })

  def verifyParent = {
    parent.map(_.children.exists(isomorphicTo_?)).getOrElse(true)
  }
  def verifyAncestry = ancestry.forall(_.verifyParent)

  def verifyAncestryForSomeIsomorph = isomorphs.exists(_.verifyAncestry)

  def verifyUpperOrbits = {
    (for (o <- upperObjects.orbits.iterator; s <- o.elements.subsets(2); Seq(a, b) = s.toSeq) yield {
      a.result.isomorphicTo_?(b.result)
    }) ++
      (for (s <- upperObjects.orbits.subsets(2); Seq(o1, o2) = s.toSeq; u1 = o1.representative; u2 = o2.representative) yield {
        !u1.result.isomorphicTo_?(u2.result)
      })
  }

}
