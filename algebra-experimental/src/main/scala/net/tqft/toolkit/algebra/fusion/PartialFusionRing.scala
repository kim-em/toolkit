package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.collections.MapTransformer
import net.tqft.toolkit.amazon.S3
import net.tqft.toolkit.SHA1
import net.tqft.toolkit.Logging
import net.tqft.toolkit.algebra.Longs
import net.tqft.toolkit.algebra.Integers
import net.tqft.toolkit.algebra.grouptheory.Orbit
import net.tqft.toolkit.permutations.Permutations.Permutation

import scala.language.implicitConversions

import scala.language.implicitConversions

object PartialFusionRing {
  implicit def fromFusionRing(ring: FusionRing[Int]): PartialFusionRing = {
    val generators = ring.minimalGeneratingSets.toSeq.sortBy(_.size).head
//    val generator = ring.sum(generators.map(ring.basis))
    val depth = ring.depthWithRespectTo(generators).max
    val relabelledRing = ring.relabelForGenerators(generators)
    PartialFusionRing(depth, (1 to generators.size).toSet, relabelledRing, ring.globalDimensionLowerBound.ceil)
  }
}

object LazyPair {
  def apply[A, B](a: => A, b: => B): Product2[A, B] = {
    new Product2[A, B] {
      override lazy val _1 = a
      override lazy val _2 = b
      override def hashCode = (_1, _2).hashCode
      override def equals(other: Any) = (_1, _2).equals(other)
      override def canEqual(other: Any) = other.isInstanceOf[Product2[A, B]]
    }
  }
}

object PartialFusionRingMeasurement {
  private val completeInvariantCases = 3
  private val completeInvariantCounts = Array.fill(completeInvariantCases)(0)
  private val completeInvariantTimings = Array.fill(completeInvariantCases)(0L)

  def reportTiming[A](k: Int, f: => A): A = {
    import net.tqft.toolkit.Profiler._
    val (t, r) = timing(f)
    completeInvariantTimings(k) = completeInvariantTimings(k) + t
    completeInvariantCounts(k) = completeInvariantCounts(k) + 1
    if (completeInvariantCounts(k) % 100 == 0) {
      println("completeInvariant summary: " + completeInvariantCounts.toList + " " + completeInvariantTimings.toList)
    }
    r
  }

}

case class PartialFusionRing(depth: Int, generators: Set[Int], ring: FusionRing[Int], globalDimensionLimit: Double) extends CanonicalGeneration[PartialFusionRing, IndexedSeq[Int]] { pfr =>
  require(ring.independent_?(generators))

  override lazy val hashCode = (depth, generators, ring, globalDimensionLimit).hashCode

  // TODO this is taking up too much memory! How about a list of functions, and whoever is sorting and splitting can take care of saving values as needed.
  lazy val completeInvariant = {
    import net.tqft.toolkit.collections.Tally._
    import net.tqft.toolkit.collections.LexicographicOrdering._

    import PartialFusionRingMeasurement.reportTiming

    def rowSums = ring.structureCoefficients.map(m => m.entries.map(_.sum).seq.tally.sorted)
    // it seems from profiling that columnSums is completely redundant with rowSums

    def rowSumsBy[A: Ordering](data: Seq[A]): Seq[Int] = {
      ring.structureCoefficients.map({ m =>
        m.entries.map({ r =>
          r.zip(data).map({
            case (rx, d) => rx * d.hashCode
          }).sum
        }).seq.zip(data).map(_.hashCode).tally.sorted
      }).zip(data).map(_.hashCode)
    }

    def rowSumsIterator = Iterator.iterate(depths.map(Integers.power(10, _)))(rowSumsBy)
    def rowSumsWithTalliesIterator = rowSumsIterator.map(_.tally.map(_.swap).sorted)
    def fixedPointLabels = rowSumsWithTalliesIterator.sliding(2, 1).find({ case Seq(p1, p2) => p1.map(_._1) == p2.map(_._1) }).get.head.map(_._2)

    def canonicalize = {
      import net.tqft.toolkit.algebra.graphs.Dreadnaut
      Dreadnaut.canonicalize(ring.graphEncoding(depths))
    }

    // about a third of cases are still going through to canonicalize
    LazyPair(
      reportTiming(0, rowSums.tally.sorted),
      LazyPair(
        reportTiming(1, fixedPointLabels),
        reportTiming(2, canonicalize)))
  }

//  override def children = {
//    try {
//      PartialFusionRingCache.getOrElseUpdate(this, super.children)
//    } catch {
//      case e: java.lang.ExceptionInInitializerError => {
//        Logging.error("S3 not available: ", e)
//        super.children
//      }
//    }
//  }

  private def generator = {
    ring.sum(for (i <- generators) yield ring.basis(i))
  }
  val depths = {
    if (ring.rank == 1) {
      Seq(0)
    } else {
      val result = ring.depthWithRespectTo(generators)
      if (result.contains(-1) || result.max < depth - 1 || result.max > depth) {
        println("depths " + result + " invalid in PartialFusionRing(" + depth + ", " + generators + ", " + ring + ", " + globalDimensionLimit + ")")
        require(false)
      }
      result
    }
  }

  lazy val automorphisms = ring.automorphisms(depths)

  override def findIsomorphismTo(other: PartialFusionRing) = {
    // FIXME implement via dreadnaut
    if (depth == other.depth && globalDimensionLimit == other.globalDimensionLimit && (ring.globalDimensionLowerBound - other.ring.globalDimensionLowerBound).abs < 0.0001) {
      import net.tqft.toolkit.permutations.Permutations
      Permutations.preserving(depths).find(p => ring.relabel(p) == other.ring)
    } else {
      None
    }
  }

  override def isomorphs: Iterator[PartialFusionRing] = {
    import net.tqft.toolkit.permutations.Permutations
    for (p <- Permutations.preserving(depths).iterator) yield copy(ring = ring.relabel(p))
  }

  // TODO refine this via easier invariants
  val ordering: Ordering[lowerObjects.Orbit] = {
    import net.tqft.toolkit.collections.Orderings._
    import net.tqft.toolkit.collections.LexicographicOrdering._
    import net.tqft.toolkit.collections.Tally._
    import net.tqft.toolkit.algebra.graphs.Dreadnaut
    
    // We work via a representative --- but have to be careful this doesn't rely on choices!
    implicit val lowerOrdering = Ordering.by({ l: Lower =>
      l match {
        case ReduceDepth => 0
        case DeleteSelfDualObject(_) => 1
        case DeleteDualPairOfObjects(_, _) => 2
      }
    }).refineBy({
      case DeleteSelfDualObject(k) => - ring.dimensionLowerBounds(ring.basis(k))
      case DeleteDualPairOfObjects(k1, k2) => - scala.math.max(ring.dimensionLowerBounds(ring.basis(k1)), ring.dimensionLowerBounds(ring.basis(k2)))
    }).refineByPartialFunction({
      // TODO would it help to do the DeleteDualPairOfObjects case??
      case DeleteSelfDualObject(k) => {
        (ring.structureCoefficients(k).entries(k)(k),
          ring.structureCoefficients(k).entries(k).tally.sorted,
          ring.structureCoefficients(k).entries.map(_(k)).seq.tally.sorted,
          ring.structureCoefficients(k).entries.flatten.seq.tally.sorted)
      }
    }).refineByPartialFunction({
      case DeleteSelfDualObject(k) => Dreadnaut.canonicalize(ring.graphEncoding(depths.updated(k, -1)))
      case d @ DeleteDualPairOfObjects(_, _) => Dreadnaut.canonicalize(ring.graphEncoding(depths.updated(d.k1, -1).updated(d.k2, -1)))
    })
    Ordering.by({ o: lowerObjects.Orbit => o.representative })
  }

  sealed trait Lower {
    def result: PartialFusionRing
  }
  case object ReduceDepth extends Lower {
    override def result = copy(depth = depth - 1)
  }
  case class DeleteSelfDualObject(k: Int) extends Lower {
    override def result = {
      import net.tqft.toolkit.collections.Deleted._
      val newGenerators = generators collect {
        case i if i < k => i
        case i if i > k => i - 1
      }
      val newRing = FusionRing(ring.structureCoefficients.deleted(k).map(_.dropRows(Seq(k)).dropColumns(Seq(k))))
      pfr.copy(ring = newRing, generators = newGenerators)
    }

  }
  case class DeleteDualPairOfObjects(k1: Int, k2: Int) extends Lower {
    require(ring.duality(k1) == k2)
    require(k1 != k2)
    override def result = {
      import net.tqft.toolkit.collections.Deleted._
      val newGenerators = generators collect {
        case i if i < k1 && i < k2 => i
        case i if i < k1 && i > k2 || i > k1 && i < k2 => i - 1
        case i if i > k1 && i > k2 => i - 2
      }
      val newRing = FusionRing(ring.structureCoefficients.deleted(Seq(k1, k2)).map(_.dropRows(Seq(k1, k2)).dropColumns(Seq(k1, k2))))
      pfr.copy(ring = newRing, generators = newGenerators)
    }
  }

  override lazy val lowerObjects = new automorphisms.Action[Lower] {
    override def elements = {
      if (ring.rank > 1) {
        if (depth == depths.max) {
          depths.zipWithIndex.iterator.collect({
            case (d, i) if d == depth && ring.duality(i) == i => DeleteSelfDualObject(i)
            case (d, i) if d == depth && ring.duality(i) > i => DeleteDualPairOfObjects(i, ring.duality(i))
          })
        } else {
          Iterator(ReduceDepth)
        }
      } else {
        Iterator.empty
      }
    }
    override def act(a: IndexedSeq[Int], b: Lower): Lower = {
      val ai = {
        import net.tqft.toolkit.permutations.Permutations._
        a.inverse
      }
      b match {
        case ReduceDepth => ReduceDepth
        case DeleteSelfDualObject(k) => DeleteSelfDualObject(ai(k))
        case DeleteDualPairOfObjects(k1, k2) => DeleteDualPairOfObjects(ai(k1), ai(k2))
      }
    }
  }

  sealed trait Upper {
    val result: PartialFusionRing
    def inverse: result.Lower
  }
  case object IncreaseDepth extends Upper {
    override lazy val result = pfr.copy(depth = depth + 1)
    override def inverse = result.ReduceDepth
  }
  trait NewRingUpper extends Upper {
    def newRing: FusionRing[Int]
  }
  case class AddSelfDualObject(newRing: FusionRing[Int]) extends NewRingUpper {
    private def newGenerators = {
      if (depth == 1) (1 until newRing.rank).toSet else generators
    }
    override lazy val result = pfr.copy(ring = newRing, generators = newGenerators)
    override def inverse = result.DeleteSelfDualObject(ring.rank)
  }
  case class AddDualPairOfObjects(newRing: FusionRing[Int]) extends NewRingUpper {
    private def newGenerators = {
      if (depth == 1) (1 until newRing.rank).toSet else generators
    }
    override lazy val result = pfr.copy(ring = newRing, generators = newGenerators)
    override def inverse = result.DeleteDualPairOfObjects(ring.rank, ring.rank + 1)
  }

  def upperObjects: automorphisms.Action[Upper] = new automorphisms.Action[Upper] {
    override def orbits: Set[Orbit] = {
      // FIXME don't generate all the elements, then compute orbits --- just prepare orbits directly!
      Logging.info("Preparing orbits of upper objects; automorphism group generated by:" + automorphisms.generators)

      super.orbits
    }

    override lazy val elements: Iterator[Upper] = {
      (if (depth == depths.max && ring.partialAssociativityConstraints(depth + 1, depths).forall(p => p._1 == p._2)) {
        Iterator(IncreaseDepth)
      } else {
        Iterator.empty
      }) ++ (if (depth > 0) {

        // FIXME independence should really be done as equations in withAnother...
        def independent_?(f: FusionRing[Int]) = {
          f.independent_?(if (depth == 1) {
            (1 until f.rank).toSet
          } else {
            generators
          })
        }

        def chooseRepresentatives(uppers: Seq[Upper]) = {

          println("Choosing representatives amongst " + uppers.size + " objects")

          import net.tqft.toolkit.collections.LexicographicOrdering._
          import net.tqft.toolkit.collections.Tally._

          //          val ordering = Ordering.by({ u: Upper => u.result.completeInvariant })
          //          import net.tqft.toolkit.collections.Split._
          //          val splits = uppers.splitByOrdering(ordering)
          //          val result = splits.map(_.head).toSet
          //          result

          import net.tqft.toolkit.collections.GroupBy._
          uppers.lazilyGroupBy({ u: Upper => u.result.completeInvariant }).map(_._2.next).toSet
        }

        val extraSelfDual = FusionRings.withAnotherSelfDualObject(ring, depth, depths, globalDimensionLimit).filter(independent_?).toSeq
//        println("self-dual: " + extraSelfDual.size)
        val extraSelfDual2 = extraSelfDual.filter({ r => 
        	val dims = for(x <- r.basis) yield r.dimensionLowerBounds(x)
        	dims.forall(_ <= dims.last)
        })        
        
        val extraPairs = FusionRings.withAnotherPairOfDualObjects(ring, depth, depths, globalDimensionLimit).filter(independent_?).toSeq
//        println("pairs: " + extraPairs.size)

        val extraPairs2 = extraPairs.filter({ r => 
        	val dims = for(x <- r.basis) yield r.dimensionLowerBounds(x)
        	val m = scala.math.max(dims(dims.size - 1), dims(dims.size - 2))
        	dims.forall(_ <= m)
        })  
        
        val extraSelfDualReps = chooseRepresentatives(extraSelfDual.map(AddSelfDualObject))
        println("self-dual: " + extraSelfDual.size + " ---> " + extraSelfDual2.size + " ---> " + extraSelfDualReps.size)
        val extraPairsReps = chooseRepresentatives(extraPairs.map(AddDualPairOfObjects))
        println("pairs: " + extraPairs.size + " ---> " + extraPairs2.size + " ---> " + extraPairsReps.size)
        (extraSelfDualReps ++ extraPairsReps).iterator
      } else {
        Iterator.empty
      })
    }
    override def act(a: IndexedSeq[Int], b: Upper): Upper = {
      b match {
        case IncreaseDepth => IncreaseDepth
        case AddSelfDualObject(newRing) => AddSelfDualObject(newRing.relabel(a :+ ring.rank))
        case AddDualPairOfObjects(newRing) => AddDualPairOfObjects(newRing.relabel(a :+ ring.rank :+ (ring.rank + 1)))
      }
    }
  }
}

private object C {
  def m2s(m: Matrix[Int]): String = {
    m.entries.map(_.mkString("{", ", ", "}")).mkString("{", ", ", "}")
  }

  def s2m(s: String): Matrix[Int] = {
    require(s.startsWith("{{"))
    require(s.endsWith("}}"))
    s.stripPrefix("{{").stripSuffix("}}").split("\\}, \\{").map(_.split(", ").map(_.toInt).toIndexedSeq).toIndexedSeq
  }

  def pfr2s(pfr: PartialFusionRing): String = {
    "depth = " + pfr.depth + "\n" +
      "generators = " + pfr.generators.mkString("Seq(", ", ", ")") + "\n" +
      "globalDimensionLimit = " + pfr.globalDimensionLimit + "\n" +
      (for (m <- pfr.ring.structureCoefficients) yield {
        m2s(m)
      }).mkString("\n") + "\n"
  }
  def pfrseq2s(seq: Seq[PartialFusionRing]): String = {
    seq.map(pfr2s).mkString("---\n", "---\n", "---\n")
  }

  def s2pfr(s: String) = {
    val depthString :: generatorsString :: dimensionString :: matricesStrings = s.split("\n").toList
    val generators = generatorsString.stripPrefix("generators = Seq(").stripSuffix(")").split(",").map(_.trim.toInt)
    PartialFusionRing(depthString.stripPrefix("depth = ").toInt, generators.toSet, FusionRing(matricesStrings.map(s2m)), dimensionString.stripPrefix("globalDimensionLimit = ").toDouble)
  }

  def s2pfrseq(s: String) = {
    s.split("---").map(_.trim).filter(_.nonEmpty).map(s2pfr).toIndexedSeq
  }
}

object PartialFusionRingCache extends MapTransformer.KeyTransformer[PartialFusionRing, String, Seq[PartialFusionRing]](new MapTransformer.ValueTransformer[String, String, Seq[PartialFusionRing]](S3/*.withAccount("AKIAIUHOCNVIDRMVRC2Q")*/("partial-fusion-ring"),
  C.s2pfrseq _,
  C.pfrseq2s _),
  { s: String => None },
  { p => SHA1(C.pfr2s(p)) })
