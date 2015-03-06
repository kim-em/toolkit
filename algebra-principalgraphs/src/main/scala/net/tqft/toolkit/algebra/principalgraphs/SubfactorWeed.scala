package net.tqft.toolkit.algebra.principalgraphs

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.permutations.Permutations._
import net.tqft.toolkit.permutations.Involutions
import net.tqft.toolkit.permutations.Involutions._
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.enumeration.Odometer
import net.tqft.toolkit.algebra.graphs._
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import scala.collection.mutable.ListBuffer
import net.tqft.toolkit.permutations.Permutations
import net.tqft.toolkit.Logging
import net.tqft.toolkit.algebra.enumeration.CanonicalGenerationWithIsomorphism

sealed trait SubfactorWeed extends CanonicalGenerationWithIsomorphism[SubfactorWeed, Seq[(Permutation, Permutation)]] { weed =>
  def indexLimit: Double
  def pair: PairOfBigraphsWithDuals

  override def toString = s"weed[$indexLimit, $pair]"
  override def isomorphicTo_?(other: SubfactorWeed) = {
    indexLimit == other.indexLimit && depth == other.depth &&
      pair.canonicalNautyGraph == other.pair.canonicalNautyGraph
  }

  def supertransitivity = pair.supertransitivity

  def descendantsFiltered(supertransitivityBound: Int = -1, depthBound: Int = -1, rankBound: Int = -1, avoiding: Seq[PairOfBigraphsWithDuals] = Seq.empty, stopWhenPersistentlyCylindrical: Boolean = true) = descendantsTreeFiltered(supertransitivityBound, depthBound, rankBound, avoiding, stopWhenPersistentlyCylindrical).map(_._1)
  def descendantsTreeFiltered(supertransitivityBound: Int = -1, depthBound: Int = -1, rankBound: Int = -1, avoiding: Seq[PairOfBigraphsWithDuals] = Seq.empty, stopWhenPersistentlyCylindrical: Boolean = true) = {
    val canonicalAvoiding = {
      avoiding.map(_.invariant) ++
        avoiding.map(_.switch.invariant)
    }
    descendantsTree(w => {
      //            println(s"beginning filtering for $w")
      if (!stopWhenPersistentlyCylindrical || !w.pair.persistentlyCylindrical_?) {
        if (supertransitivityBound <= 0 || w.supertransitivity <= supertransitivityBound || w.supertransitivity == w.depth && w.depth == supertransitivityBound + 1) {
          if ((rankBound <= 0 || w.pair.totalRank <= rankBound) && (depthBound <= 0 || w.pair.depth <= depthBound)) {
            if (canonicalAvoiding.contains((w.depth, w.pair.nautyGraph)) || canonicalAvoiding.contains(w.pair.invariant)) {
              //                            println(s"  rejected $w, as it's on the ignoring list: ${w.pair.canonicalNautyGraph}")
              -1
            } else {
              //              println("  accepted!")
              1
            }
          } else {
            -1
          }
        } else {
          -1
        }
      } else {
        // if persistently cylindrical, report it!
        0
      }
    })
  }

  override def findIsomorphismTo(other: SubfactorWeed) = {
    if (depth == other.depth) {
      Dreadnaut.findIsomorphism(pair.nautyGraph, other.pair.nautyGraph).map(splitPermutation)
    } else {
      None
    }
  }
  def isomorphs = ???

  override def verifyParent = {
    val r = super.verifyParent
    if (!r) {
      println(s"Parent verification failed for:\n$this")
      println(s"Parent:\n${parent.get}")
      println("Children of parent:")
      for (c <- parent.get.children) {
        println("  " + c)
      }
      println("Nauty graph:")
      println(pair.nautyGraph)
      println("Dreadnaut output:")
      println(Dreadnaut.invokeDreadnaut(pair.nautyGraph.toDreadnautString + " cxo\n").mkString("\n"))
    }
    r
  }

  protected def depth = pair(0).bigraph.depth

  private def splitPermutation(p: Permutation): Seq[(Permutation, Permutation)] = {
    require(p(0) == 0)
    def shift(x: IndexedSeq[Int]) = {
      if (x.isEmpty) {
        x
      } else {
        val m = x.min
        x.map(_ - m)
      }
    }
    val chunks = pair.rankPartialSums.sliding(2).map(t => p.slice(t(0), t(1))).map(shift).toIndexedSeq
    for (i <- 0 to depth) yield {
      (chunks(i), chunks(i + depth + 1))
    }
  }

  override lazy val automorphisms: FinitelyGeneratedFiniteGroup[Seq[(Permutation, Permutation)]] = {
    val group = Dreadnaut.automorphismGroup(pair.nautyGraph)

    FiniteGroups.indexedProduct(
      for (i <- 0 until pair.g0.bigraph.depth) yield {
        FiniteGroups.product(
          FiniteGroups.symmetricGroup(pair.g0.bigraph.rankAtDepth(i)),
          FiniteGroups.symmetricGroup(pair.g1.bigraph.rankAtDepth(i)))
      }).subgroupGeneratedBy(group.generators.map(splitPermutation))
  }

  sealed trait Upper {
    val result: SubfactorWeed
    def inverse: result.Lower

    def associative_? : Boolean
  }
  case object IncreaseDepth extends Upper {
    override lazy val result = SubfactorWeed(indexLimit, pair.increaseDepth)
    override def inverse = result.DecreaseDepth

    override def associative_? = pair.associativeAtPenultimateDepth_?
  }

  def decreaseDepthSet: Option[Stream[Lower]] = (pair.depth > 1 && pair.g0.bigraph.rankAtMaximalDepth == 0 && pair.g1.bigraph.rankAtMaximalDepth == 0) option Stream(DecreaseDepth)

  sealed trait Lower {
    def result: SubfactorWeed
  }
  case object DecreaseDepth extends Lower {
    override def result = SubfactorWeed(indexLimit, pair.truncate)
  }
}

object SubfactorWeed {
  def apply(indexLimit: Double, pair: PairOfBigraphsWithDuals): SubfactorWeed = {
    pair match {
      case pair: EvenDepthPairOfBigraphsWithDuals => EvenDepthSubfactorWeed(indexLimit, pair)
      case pair: OddDepthPairOfBigraphsWithDuals => OddDepthSubfactorWeed(indexLimit, pair)
    }
  }

  var experimental = false
}

case class EvenDepthSubfactorWeed(indexLimit: Double, pair: EvenDepthPairOfBigraphsWithDuals) extends SubfactorWeed { weed =>

  override lazy val lowerObjects = {
    new automorphisms.Action[Lower] {
      override val elements: Stream[Lower] =
        decreaseDepthSet.getOrElse(
          (for (i <- (0 to 1).toStream; k <- 0 until pair(i).bigraph.rankAtMaximalDepth; if pair(i).dualData.last(k) != k - 1) yield {
            if (pair(i).dualData.last(k) == k) {
              DeleteSelfDualVertex(i, k)
            } else {
              DeleteDualPairAtEvenDepth(i, k)
            }
          }))

      override def act(g: Seq[(Permutation, Permutation)], x: Lower): Lower = x match {
        case DecreaseDepth => x
        case DeleteSelfDualVertex(graph, index) => DeleteSelfDualVertex(graph, (graph match { case 0 => g.last._1; case 1 => g.last._2 })(index))
        case DeleteDualPairAtEvenDepth(graph, index) => {
          val newIndex0 = {
            (graph match { case 0 => g.last._1; case 1 => g.last._2 })(index)
          }
          val newIndex1 = if (pair(graph).dualData.last(newIndex0) < newIndex0) {
            newIndex0 - 1
          } else {
            newIndex0
          }
          DeleteDualPairAtEvenDepth(graph, newIndex1)
        }
      }
    }
  }
  override def ordering: Ordering[lowerObjects.Orbit] = {
    import Ordering.Implicits._
    import net.tqft.toolkit.orderings.Orderings._

    implicit val lowerOrdering: Ordering[Lower] = Ordering.by({ l: Lower =>
      l match {
        case DecreaseDepth => 0
        case DeleteDualPairAtEvenDepth(1, _) => 1
        case DeleteSelfDualVertex(1, _) => 2
        case DeleteDualPairAtEvenDepth(0, _) => 3
        case DeleteSelfDualVertex(0, _) => 4
        case _ => ???
      }
    }).refineByPartialFunction({
      case DeleteSelfDualVertex(graph, index) => pair(graph).bigraph.inclusions.last(index).sum
    }).refineByPartialFunction({
      case DeleteDualPairAtEvenDepth(graph, index) => 
         Seq(pair(graph).bigraph.inclusions.last(index).sum, pair(graph).bigraph.inclusions.last(index + 1).sum).sorted
    }).refineByPartialFunction({
      case DeleteSelfDualVertex(graph, index) => {
        import net.tqft.toolkit.collections.Tally._
        pair(graph).bigraph.downUpNeighbours(pair(graph).bigraph.depth, index).tally.values.toSeq.sorted
      }
    }) .refineByPartialFunction({
      case DeleteDualPairAtEvenDepth(graph, index) => {
        import net.tqft.toolkit.collections.Tally._
        Seq(pair(graph).bigraph.downUpNeighbours(pair(graph).bigraph.depth, index).tally.values.toSeq.sorted,
          pair(graph).bigraph.downUpNeighbours(pair(graph).bigraph.depth, index + 1).tally.values.toSeq.sorted).sorted
      }
    }) .refineByPartialFunction({
        case DeleteSelfDualVertex(graph, index) => Dreadnaut.canonicalize(pair.nautyGraph.additionalMarking(Seq(pair.graphLabel(graph, pair(graph).bigraph.depth, index))))
        case DeleteDualPairAtEvenDepth(graph, index) => Dreadnaut.canonicalize(pair.nautyGraph.additionalMarking(Seq(pair.graphLabel(graph, pair(graph).bigraph.depth, index), pair.graphLabel(graph, pair(graph).bigraph.depth, index + 1))))
      })
    Ordering.by({ o: lowerObjects.Orbit => o.representative })
  }

  override def upperObjects = {
    new automorphisms.Action[Upper] {
      override val elements: Stream[Upper] = {
        val allUppers: Iterator[Upper] = {
          def uppersAddingVerticesToGraph(graph: Int): Iterator[Upper] = {
            if (graph == 0 && pair.g1.bigraph.rankAtMaximalDepth > 0) {
              Iterator.empty
            } else {
              uppersAddingSelfDualVerticesToGraph(graph) ++
                uppersAddingDualPairVerticesToGraph(graph)
            }
          }

          def uppersAddingSelfDualVerticesToGraph(graph: Int): Iterator[Upper] = {
            val duals = pair(graph).dualData.last
            if (duals.nonEmpty && duals.last == duals.size - 2 /* graph has dual pairs already */ ) {
              Iterator.empty
            } else {
              // Now we have to run an odometer to produce all values of the row.
              // We certainly give this odometer the index limit.
              // What sort of limits can we give the odometer?
              // --->  Given any function f: Seq[Int] => (L: Ordering) which is pointwise increasing and which is automorphism invariant,
              //       we can use the limit f(row) <= rows.map(f).min
              // Examples:
              // * sum of the entries
              // * tally of neighbour degrees (if vertex i at the previous depth has valence n_i, we send row to (n \mapsto \sum_{i: n_i = n} row_i), ordered lexicographically
              // 
              // Of course, any limits that are used here, have to be reflected in the ordering of lowers.

              val limit = { row: List[Int] =>
                // FIXME we're only doing the simply laced case for now
                val result = row.forall(_ <= 1) && (pair.depth <= 1 || !pair.truncate.cylindrical_? || (row.sum <= 1 && !pair(graph).bigraph.inclusions.last.contains(row))) &&
                  pair(graph).bigraph.isEigenvalueWithRowBelow_?(indexLimit)(row)
                //                if (result) {
                //                  Logging.info(s"  considering new row (on graph $graph): " + row.mkString("x"))
                //                } else {
                //                  Logging.info(s"  rejecting   new row on $graph: " + row.mkString("x"))
                //                }
                result
              }

              val initial = List.fill(pair(graph).bigraph.rankAtDepth(-2))(0)

              Odometer(limit)(initial).filter(r => r.exists(_ > 0)).map(AddSelfDualVertex(graph, _))
            }
          }

          def uppersAddingDualPairVerticesToGraph(graph: Int): Iterator[Upper] = {
            def limit(bigraph: Bigraph) = { row: List[Int] =>
              // FIXME we're only doing the simply laced case for now
              row.forall(_ <= 1) && (pair.depth <= 1 || !pair.truncate.cylindrical_? || (row.sum <= 1 && !bigraph.inclusions.last.contains(row))) &&
                bigraph.isEigenvalueWithRowBelow_?(indexLimit)(row)
            }
            val firstLimit = { row0: List[Int] =>
              val result = limit(pair(graph).bigraph)(row0)
              //              if (result) {
              //                Logging.info(s"  considering new row 0 (on graph $graph): " + row0.mkString("x"))
              //              } else {
              //                Logging.info(s"  rejecting   new row 0 (on graph $graph): " + row0.mkString("x"))
              //              }
              result
            }
            def secondLimit(row0: List[Int], bigraph: Bigraph) = { row1: List[Int] =>
              /* FIXME be careful; the odometer is working in backwards lexicographic order!!! */
              import Ordering.Implicits._

              // 
              val result = row1 <= row0 && limit(bigraph)(row1)
              //              if (result) {
              //                Logging.info(s"  considering new row 1 (on graph $graph): " + row1.mkString("x") + " (with row 0: " + row0.mkString("x") + ")")
              //              } else {
              //                Logging.info(s"  rejecting   new row 1 (on graph $graph): " + row1.mkString("x") + " (with row 0: " + row0.mkString("x") + ")")
              //              }
              result
            }

            val initial = List.fill(pair(graph).bigraph.rankAtDepth(-2))(0)

            Odometer(firstLimit)(initial)
              .filter(r => r.exists(_ > 0))
              .map(row => (row, pair(graph).bigraph.addRow(row)))
              .flatMap(p => Odometer(secondLimit(p._1, p._2))(initial).filter(r => r.exists(_ > 0)).map(r => (p._1, r)))
              .map(p => AddDualPairAtEvenDepth(graph, p._1, p._2))
          }

          val dualDataAllowed = depth > supertransitivity + 1 || pair.g0.numberOfSelfDualObjectsAtMaximalDepth == pair.g1.numberOfSelfDualObjectsAtMaximalDepth
          val increaseDepthAllowed = (pair.g0.bigraph.rankAtMaximalDepth > 0 && pair.g1.bigraph.rankAtMaximalDepth > 0) && dualDataAllowed

          (increaseDepthAllowed option IncreaseDepth).iterator ++
            uppersAddingVerticesToGraph(0) ++
            uppersAddingVerticesToGraph(1)
        }

        allUppers.filter(_.associative_?).filter(_.result.pair.passesTriplePointObstruction_?).toStream
      }
      override def act(g: Seq[(Permutation, Permutation)], x: Upper): Upper = x match {
        case IncreaseDepth => x
        case AddSelfDualVertex(graph, row) => AddSelfDualVertex(graph, (graph match { case 0 => g.secondLast._1; case 1 => g.secondLast._2 }).permute(row))
        case AddDualPairAtEvenDepth(graph, row0, row1) => {
          val p = (graph match { case 0 => g.secondLast._1; case 1 => g.secondLast._2 })
          val nr0 = p.permute(row0)
          val nr1 = p.permute(row1)
          import Ordering.Implicits._
          if (nr1 <= nr0) {
            AddDualPairAtEvenDepth(graph, nr0, nr1)
          } else {
            AddDualPairAtEvenDepth(graph, nr1, nr0)
          }
        }
      }
    }
  }

  sealed trait AddVertexUpper extends Upper {
    def pairOption: Option[EvenDepthPairOfBigraphsWithDuals]
    def associative_? = pairOption.nonEmpty
    override lazy val result = EvenDepthSubfactorWeed(indexLimit, pairOption.get)
  }

  case class AddSelfDualVertex(graph: Int, row: Seq[Int]) extends AddVertexUpper {
    override lazy val pairOption = pair.addSelfDualVertex(graph, row)
    override def inverse = result.DeleteSelfDualVertex(graph, pair(graph).bigraph.rankAtMaximalDepth)
  }  
  case class AddDualPairAtEvenDepth(graph: Int, row0: Seq[Int], row1: Seq[Int]) extends AddVertexUpper {
    override lazy val pairOption = pair.addDualPairAtEvenDepth(graph, row0, row1)
    override def inverse = result.DeleteDualPairAtEvenDepth(graph, pair(graph).bigraph.rankAtMaximalDepth)
  }

  case class DeleteSelfDualVertex(graph: Int, index: Int) extends Lower {
    override def result = EvenDepthSubfactorWeed(indexLimit, pair.deleteSelfDualVertex(graph, index))
  }
  case class DeleteDualPairAtEvenDepth(graph: Int, index: Int) extends Lower {
    require(index < pair(graph).bigraph.rankAtMaximalDepth - 1)
    override def result = EvenDepthSubfactorWeed(indexLimit, pair.deleteDualPairAtEvenDepth(graph, index))
  }
}

case class OddDepthSubfactorWeed(indexLimit: Double, pair: OddDepthPairOfBigraphsWithDuals) extends SubfactorWeed { weed =>

  override lazy val lowerObjects = {
    new automorphisms.Action[Lower] {
      override val elements: Stream[Lower] =
        decreaseDepthSet.getOrElse(
          for (k <- (0 until pair.g0.bigraph.rankAtMaximalDepth).iterator.toStream) yield DeleteDualPairAtOddDepth(k))

      def act(g: Seq[(Permutation, Permutation)], x: Lower): Lower = x match {
        case DecreaseDepth => x
        case DeleteDualPairAtOddDepth(index: Int) => DeleteDualPairAtOddDepth(g.last._1(index))
      }
    }
  }
  override def ordering: Ordering[lowerObjects.Orbit] = {
    import Ordering.Implicits._
    import net.tqft.toolkit.orderings.Orderings._

    implicit val lowerOrdering: Ordering[Lower] = Ordering.by({ l: Lower =>
      l match {
        case DecreaseDepth => 0
        case DeleteDualPairAtOddDepth(_) => 1
      }
    }).refineByPartialFunction({
      case DeleteDualPairAtOddDepth(index) => (pair(0).bigraph.inclusions.last(index).sum, pair(1).bigraph.inclusions.last(index).sum)
    }).refineByPartialFunction({
      case DeleteDualPairAtOddDepth(index) => {
        import net.tqft.toolkit.collections.Tally._
        (pair(0).bigraph.downUpNeighbours(pair(0).bigraph.depth, index).tally.values.toSeq.sorted,
          pair(1).bigraph.downUpNeighbours(pair(1).bigraph.depth, index).tally.values.toSeq.sorted)
      }
    }).refineByPartialFunction({
      case DeleteDualPairAtOddDepth(index) => {
        Dreadnaut.canonicalize(pair.nautyGraph.additionalMarking(Seq(pair.graphLabel(0, pair(0).bigraph.depth, index), pair.graphLabel(1, pair(1).bigraph.depth, index))))
      }
    })
    Ordering.by({ o: lowerObjects.Orbit => o.representative })
  }

  override def upperObjects: automorphisms.Action[Upper] = {
    new automorphisms.Action[Upper] {
      override val elements: Stream[Upper] = {
        def uppersAddingVerticesToGraph: Iterator[Upper] = {

          def limit(graph: Int) = { row: List[Int] =>
            row.forall(_ <= 1) && (!pair.truncate.cylindrical_? || (row.sum <= 1 && !pair(graph).bigraph.inclusions.last.contains(row))) && {
              val result = pair(graph).bigraph.isEigenvalueWithRowBelow_?(indexLimit)(row)
              //              if (result) {
              //                Logging.info(s"  considering new row $graph: " + row.mkString("x"))
              //              } else {
              //                Logging.info(s"  rejecting   new row $graph: " + row.mkString("x"))
              //              }
              result
            }
          }
          def initial(graph: Int) = List.fill(pair(graph).bigraph.rankAtDepth(-2))(0)
          Odometer(limit(0))(initial(0))
            .filter(_.exists(_ > 0))
            .flatMap(row0 => Odometer(limit(1))(initial(1)).map(row1 => (row0, row1)))
            .map({ rows => AddDualPairAtOddDepth(rows._1, rows._2) })
        }

        val allUppers: Iterator[Upper] = ((pair.g0.bigraph.rankAtMaximalDepth > 0) option IncreaseDepth).iterator ++ uppersAddingVerticesToGraph

        allUppers.filter(_.associative_?).filter(_.result.pair.passesTriplePointObstruction_?).toStream
      }
      override def act(g: Seq[(Permutation, Permutation)], x: Upper): Upper = x match {
        case IncreaseDepth => x
        case AddDualPairAtOddDepth(row0, row1) => {
          AddDualPairAtOddDepth(g.secondLast._1.permute(row0), g.secondLast._2.permute(row1))
        }
      }
    }
  }

  sealed trait AddVertexUpper extends Upper {
    def pairOption: Option[OddDepthPairOfBigraphsWithDuals]
    def associative_? = pairOption.nonEmpty
    override lazy val result = OddDepthSubfactorWeed(indexLimit, pairOption.get)
  }

  case class AddDualPairAtOddDepth(row0: Seq[Int], row1: Seq[Int]) extends AddVertexUpper {
    override lazy val pairOption = pair.addDualPairAtOddDepth(row0, row1)
    override def inverse = result.DeleteDualPairAtOddDepth(pair.g0.bigraph.rankAtMaximalDepth)
  }
  case class DeleteDualPairAtOddDepth(index: Int) extends Lower {
    override def result = OddDepthSubfactorWeed(indexLimit, pair.deleteDualPairAtOddDepth(index))
  }

}