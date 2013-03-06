package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.collections.MapTransformer
import net.tqft.toolkit.amazon.S3
import net.tqft.toolkit.SHA1
import net.tqft.toolkit.Logging

object PartialFusionRing {
  implicit def fromFusionRing(ring: FusionRing[Int]): PartialFusionRing = {
    val generators = ring.minimalGeneratingSets.toSeq.sortBy(_.size).head
    val generator = ring.sum(generators.map(ring.basis))
    val depth = ring.depthWithRespectTo(generator).max
    val relabelledRing = ring.relabelForGenerators(generators)
    PartialFusionRing(depth, (1 to generators.size), relabelledRing, ring.globalDimensionLowerBound.ceil)
  }
}

object LazyPair {
  def apply[A, B](a: => A, b: => B): Product2[A,B] = {
    new Product2[A, B] {
      override lazy val _1 = a
      override lazy val _2 = b
      override def hashCode = (_1, _2).hashCode
      override def equals(other: Any) = (_1, _2).equals(other)
      override def canEqual(other: Any) = other.isInstanceOf[Product2[A, B]]
    }
  }
}

case class PartialFusionRing(depth: Int, generators: Seq[Int], ring: FusionRing[Int], globalDimensionLimit: Double) extends CanonicalGeneration[PartialFusionRing, IndexedSeq[Int]] { pfr =>

  override lazy val hashCode = (depth, generators, ring, globalDimensionLimit).hashCode

  lazy val completeInvariant = {
    import net.tqft.toolkit.collections.Tally._
    import net.tqft.toolkit.collections.LexicographicOrdering._
    def rowSums = ring.structureCoefficients.map(m => m.entries.map(_.sum).seq.tally.sorted).tally.sorted
    def columnSums = ring.structureCoefficients.map(m => m.entries.seq.transpose.map(_.sum).tally.sorted).tally.sorted
    
    import net.tqft.toolkit.algebra.graphs.dreadnaut
    LazyPair(rowSums, LazyPair(columnSums, dreadnaut.canonicalize(ring.graphEncoding(depths))))
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
    //    if (ring.multiply(ring.basis(1), ring.basis(1)).head == 1) {
    //      ring.basis(1)
    //    } else {
    //      ring.add(ring.basis(1), ring.basis(2))
    //    }
    ring.sum(for (i <- generators) yield ring.basis(i))
  }
  val depths = {
    if (ring.rank == 1) {
      Seq(0)
    } else {
      val result = ring.depthWithRespectTo(generator)
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
  val ordering: Ordering[Lower] = {
    import net.tqft.toolkit.collections.Orderings._
    import net.tqft.toolkit.algebra.graphs.dreadnaut
    Ordering.by({ l: Lower =>
      l match {
        case ReduceDepth => 0
        case DeleteSelfDualObject(_) => 1
        case DeleteDualPairOfObjects(_, _) => 2
      }
    }).refineByPartialFunction({
      case DeleteSelfDualObject(k) => dreadnaut.canonicalize(ring.graphEncoding(depths.updated(k, -1)))
      case d @ DeleteDualPairOfObjects(_, _) => dreadnaut.canonicalize(ring.graphEncoding(depths.updated(d.k1, -1).updated(d.k2, -1)))
    })
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

  def lowerObjects = new automorphisms.Action[Lower] {
    override def elements = {
      if (ring.rank > 1) {
        if (depth == depths.max) {
          depths.zipWithIndex.collect({
            case (d, i) if d == depth && ring.duality(i) == i => DeleteSelfDualObject(i)
            case (d, i) if d == depth && ring.duality(i) > i => DeleteDualPairOfObjects(i, ring.duality(i))
          }).toSet
        } else {
          Set(ReduceDepth)
        }
      } else {
        Set.empty
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
  case class AddSelfDualObject(newRing: FusionRing[Int]) extends Upper {
    private def newGenerators = {
      if (depth == 1) (1 until newRing.rank) else generators
    }
    override lazy val result = pfr.copy(ring = newRing, generators = newGenerators)
    override def inverse = result.DeleteSelfDualObject(ring.rank)
  }
  case class AddDualPairOfObjects(newRing: FusionRing[Int]) extends Upper {
    private def newGenerators = {
      if (depth == 1) (1 until newRing.rank) else generators
    }
    override lazy val result = pfr.copy(ring = newRing, generators = newGenerators)
    override def inverse = result.DeleteDualPairOfObjects(ring.rank, ring.rank + 1)
  }

  def upperObjects = new automorphisms.Action[Upper] {
    override lazy val elements: Set[Upper] = {
      (if (depth == depths.max && ring.partialAssociativityConstraints(depth + 1, depths).forall(p => p._1 == p._2)) {
        Set(IncreaseDepth)
      } else {
        Set.empty
      }) ++ (if (depth > 0) {
        def independent_?(f: FusionRing[Int]) = {
          f.independent_?(if (depth == 1) {
            1 until f.rank
          } else {
            generators
          })
        }

        def chooseRepresentatives(uppers: Seq[Upper]) = {
          import net.tqft.toolkit.collections.LexicographicOrdering._
          val ordering = Ordering.by({ u: Upper => u.result.completeInvariant })
          import net.tqft.toolkit.collections.Split._
          uppers.splitByOrdering(ordering).map(_.head)
          
          
//          import net.tqft.toolkit.Profiler
//          val (t0, result) = Profiler.timing(uppers.map(u => u.result.completeInvariant -> u).toMap.values)
//          //          println("t0 -> " + t0)
//          result
        }

        val extraSelfDual = FusionRings.withAnotherSelfDualObject(ring, depth, depths, globalDimensionLimit).filter(independent_?)
        val extraPairs = FusionRings.withAnotherPairOfDualObjects(ring, depth, depths, globalDimensionLimit).filter(independent_?)
        chooseRepresentatives(extraSelfDual.map(AddSelfDualObject).toSeq) ++
          chooseRepresentatives(extraPairs.map(AddDualPairOfObjects).toSeq)
      } else {
        Set.empty
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
    PartialFusionRing(depthString.stripPrefix("depth = ").toInt, generators, FusionRing(matricesStrings.map(s2m)), dimensionString.stripPrefix("globalDimensionLimit = ").toDouble)
  }

  def s2pfrseq(s: String) = {
    s.split("---").map(_.trim).filter(_.nonEmpty).map(s2pfr).toIndexedSeq
  }
}

object PartialFusionRingCache extends MapTransformer.KeyTransformer[PartialFusionRing, String, Seq[PartialFusionRing]](new MapTransformer.ValueTransformer[String, String, Seq[PartialFusionRing]](S3.withAccount("AKIAIUHOCNVIDRMVRC2Q")("partial-fusion-ring"),
  C.s2pfrseq _,
  C.pfrseq2s _),
  { s: String => None },
  { p => SHA1(C.pfr2s(p)) })
