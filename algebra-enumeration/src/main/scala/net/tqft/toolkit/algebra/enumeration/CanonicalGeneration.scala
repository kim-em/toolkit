package net.tqft.toolkit.algebra.enumeration

import net.tqft.toolkit.algebra.Group
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.Logging
import scala.language.reflectiveCalls
import CanonicalGeneration.{ info }
import scala.collection.immutable.Queue
import net.tqft.toolkit.collections.Iterators
import net.tqft.toolkit.algebra.Integers

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

  // and, for convenience, something to recursively find all children, filtering on a predicate
  private def descendantsComplete(accept: A => Double = { _ => 1 }): Iterator[A] = {
    accept(this) match {
      case a if a > 0 => {
        Iterator(this) ++ Iterator.continually(children).take(1).map(_.iterator.flatMap(_.descendants(accept))).flatten
      }
      case 0 => {
        Iterator(this)
      }
      case a if a < 0 => Iterator.empty
    }
  }

  private lazy val splittingData: (Seq[Seq[A]], IndexedSeq[Seq[A]]) = {
    val startTime = System.nanoTime
    def elapsedTimeMillis = (System.nanoTime - startTime) / 1000000
    var done: List[List[A]] = Nil
    val queue = scala.collection.mutable.Queue[List[A]](List(this))
    while (queue.nonEmpty && elapsedTimeMillis < 2000) {
      val a = queue.dequeue
      done = a :: done
      queue.enqueue(a.head.children.map(c => c :: a): _*)
    }
    val todo = queue.toIndexedSeq
    println(s"${todo.size} splitting cases available.")
    (done, todo)
  }

  def descendants(accept: A => Double = { _ => 1 }, res: Int = 0, mod: Int = 1): Iterator[A] = {
    if (mod == 1) {
      descendantsComplete(accept)
    } else {

      def acceptAncestry(x: Seq[A]) = accept(x.head) >= 0 && x.tail.forall(a => accept(a) > 0)
      val done = splittingData._1.filter(acceptAncestry).map(_.head)
      val todo = splittingData._2.filter(acceptAncestry).map(_.head)

      (if (res == 0) {
        done.iterator
      } else {
        Iterator.empty
      }) ++
        (for (
          z <- (res until todo.size by mod).iterator;
          r = todo(z);
          a <- r.descendantsComplete(accept)
        ) yield a)
    }
  }

  def dyadicDescendants(accept: A => Double = { _ => 1 }, res: Int, exponent: Int): Iterator[A] = {
    if (exponent == 0) {
      descendantsComplete(accept)
    } else {
      def thisIterator = (if (res == 0) Iterator(this) else Iterator.empty)

      accept(this) match {
        case a if a > 0 => {
          def selectChildren(c: Seq[A]): Seq[(A, Int, Int)] = {
            if (c.isEmpty) {
              Seq.empty
            } else {
//              println(s"Selecting children from $children.")
//              println(s"res = $res, exponent = $exponent")
              val log = 32 - Integer.numberOfLeadingZeros(c.size - 1) // rounding up
              val min = Math.min(log, exponent)
//              println(s"c.size = ${c.size}, log = $log, min = $min")
              val result = Integers.power(2, min) match {
                case n if n > c.size => {
                  val rn = res % n
                  if (rn < n % c.size) {
                    // we're at the beginning
                    Seq((c(rn), res / n, exponent - min + 1))
                  } else if (rn >= c.size) {
                    // we've wrapped around
                    Seq((c(rn % c.size), res / n + Integers.power(2, exponent - min), exponent - min + 1))
                  } else {
                    // we're just in the middle
                    Seq((c(rn), res / n, exponent - min))
                  }
                }
                case n if n == c.size => {
                  Seq((c(res % n), res / n, exponent - min))
                }
                case n if n < c.size => {
                  for (z <- res until c.size by n) yield (c(z), 0, 1)
                }
              }
//              for (x <- result) { println(x) }
//              println(".")
              result
            }
          }
          thisIterator ++ Iterator.continually(children).take(1).flatMap(c => selectChildren(c).map({ case (a, newRes, newExponent) => a.dyadicDescendants(accept, newRes, newExponent) })).flatten
        }
        case 0 => {
          thisIterator
        }
        case a if a < 0 => Iterator.empty
      }
    }
  }

  //  def descendantsWithETAs(accept: A => Double = { _ => 1 }, res: Int = 0, mod: Int = 1, estimateForOtherBranches: BigInt = 0): Iterator[(A, BigInt)] = {
  //    def thisIterator = if (res == 0) {
  //      Iterator((this, estimateForOtherBranches))
  //    } else {
  //      Iterator.empty
  //    }
  //
  //    accept(this) match {
  //      case a if a > 0 => {
  //        val (subset, r, m) = childrenResMod(children, res, mod)
  //        val startTime = System.nanoTime
  //        def elapsedTime = (System.nanoTime - startTime) / 1000000000
  //        thisIterator ++ subset.zipWithIndex.iterator.flatMap({ case (c, i) => c.descendantsWithETAs(accept, r, m, estimateForOtherBranches + (subset.size - i) * elapsedTime / (i + 1)) })
  //      }
  //      case 0 => {
  //        thisIterator
  //      }
  //      case a if a < 0 => Iterator.empty
  //    }
  //  }

  def descendantsTree(accept: A => Double = { _ => 1 }): Iterator[(A, Seq[A])] = {
    accept(this) match {
      case a if a > 0 => {
        val c = children
        Iterator((this, c)) ++ c.iterator.flatMap(_.descendantsTree(accept))
      }
      case 0 => Iterator((this, Nil))
      case a if a < 0 => Iterator.empty
    }
  }

  def descendantsWithProgress(accept: A => Double = { _ => 1 }): Iterator[(A, Seq[(Int, Int)])] = {
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
  def runtimeEstimators: Iterator[BigInt] = {
    def estimate = randomLineage.foldLeft((1, BigInt(0)))({ (a, b) => (a._1 * b._2.size, a._2 + a._1 * b._3) })._2
    def estimates = Iterator.continually(estimate)
    def partialSums = estimates.scanLeft(BigInt(0))(_ + _)
    def averages = partialSums.zipWithIndex.collect({ case (s, i) if i != 0 => s / i })
    averages
  }
  private def verboseDuration(x: BigInt): String = {
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
    sb.append((x % minute).toDouble / second + " seconds.")

    sb.toString
  }
  def runtimeEstimatorStrings = runtimeEstimators.map(verboseDuration)

}

trait CanonicalGenerationWithIsomorphism[A <: CanonicalGenerationWithIsomorphism[A, G], G] extends CanonicalGeneration[A, G] { this: A =>
  def findIsomorphismTo(other: A): Option[G]
  def isomorphs: Iterator[A]
  def isomorphicTo_?(other: A) = findIsomorphismTo(other).nonEmpty

  //  def descendantsTreeAvoiding(instances: Seq[A], accept: A => Int = { _ => 1 }) = descendantsTree({ a: A =>
  //    if (instances.exists(_.isomorphicTo_?(a))) {
  //      0
  //    } else {
  //      accept(a)
  //    }
  //  })

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
