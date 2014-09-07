package net.tqft.toolkit.algebra.principalgraphs

import net.tqft.toolkit.Logging
import net.tqft.toolkit.permutations.Permutations
import net.tqft.toolkit.permutations.Permutations._
import net.tqft.toolkit.permutations.Involutions
import net.tqft.toolkit.permutations.Involutions._
import net.tqft.toolkit.algebra.graphs.Graph
import net.tqft.toolkit.algebra.graphs.ColouredGraph

case class Vertex(graph: Int, depth: Int, index: Int)
case class TriplePointConfiguration(depth: Int, index0: Int, index1: Int, bijection: Seq[(Vertex, Vertex)])

trait PairOfBigraphsWithDuals {
  def g0: BigraphWithDuals
  def g1: BigraphWithDuals
  
  def switch = PairOfBigraphsWithDuals(g1, g0)
  
  def graph(g: Int): BigraphWithDuals = {
    g match {
      case 0 => g0
      case 1 => g1
    }
  }
  def dual(v: Vertex): Vertex = {
    v match {
      case Vertex(g, d, k) if d % 2 == 1 => Vertex(1 - g, d, k)
      case Vertex(g, d, k) => Vertex(g, d, graph(g).dualData(d / 2)(k))
    }
  }

  def supertransitivity = Seq(g0.supertransitivity, g1.supertransitivity).min
  def cylindrical_? = g0.bigraph.cylindrical_? || g1.bigraph.cylindrical_? || {
    supertransitivity < depth &&
      (g0.bigraph.inclusions.last ++ g0.bigraph.inclusions.last.transpose ++ g1.bigraph.inclusions.last ++ g1.bigraph.inclusions.last.transpose).map(_.sum).forall(_ <= 1)
  }

  type Clump = Int

  def associativityDefects: Memo[Seq[Seq[Int]]]
  def verticesByDimension: Memo[Seq[Seq[Seq[Clump]]]]

  def triplePointConfigurations: Memo[Seq[TriplePointConfiguration]]

  def passesTriplePointObstruction_? = {
    val obstructedTriplePointOption = triplePointConfigurations.value.find({ conf =>
      conf.bijection.forall({
        case (v1, v2) =>
          updatedVerticesByDimension(v1.graph)(v1.depth)(v1.index) == updatedVerticesByDimension(v2.graph)(v2.depth)(v2.index)
      })
    })
//    for (c <- obstructedTriplePointOption) {
//      Logging.info(s"   the triple point obstruction applies at depth ${c.depth}, vertices ${c.index0} and ${c.index1}")
//    }
    obstructedTriplePointOption.isEmpty
  }

  def newTriplePointConfigurations = {
    if (depth < 2) {
      Seq.empty
    } else {
      val d = depth - 2
      //      println(s"looking for new triple point configurations at depth = $depth")
      for (
        i <- 0 until g0.bigraph.rankAtDepth(d);
        j <- 0 until g1.bigraph.rankAtDepth(d);
        //        _ = println(s"i = $i, j = $j"); 
        if g0.bigraph.valence(d, i) == 3;
        if g1.bigraph.valence(d, j) == 3;
        //        _ = println("valences look ok!");
        if connectionsBetween((d, i), (d, j)) == 3;
        ni = g0.bigraph.neighbours(d, i);
        nj = g1.bigraph.neighbours(d, j);
        //        _ = println(s"ni = $ni, nj = $nj");
        p <- Permutations.of(3);
        pnj = p.permute(nj);
        if Seq((0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)).forall(q => connectionsBetween(ni(q._1), pnj(q._2)) == 1)
      ) yield {
        TriplePointConfiguration(d, i, j, ni.map(n => Vertex(0, n._1, n._2)).zip(pnj.map(n => Vertex(1, n._1, n._2))))
      }
    }
  }
  private def rightNeighbours(v: Vertex) = {
    graph(v.graph).bigraph.neighbours(v.depth, v.index).map(p => Vertex(v.graph, p._1, p._2))
  }
  private def leftNeighbours(v: Vertex) = {
    rightNeighbours(dual(v)).map(dual)
  }
  private def connectionsBetween(vertex0: (Int, Int), vertex1: (Int, Int)): Int = {
    val rn = rightNeighbours(Vertex(0, vertex0._1, vertex0._2))
    val ln = leftNeighbours(Vertex(1, vertex1._1, vertex1._2))
    rn.intersect(ln).size
  }

  lazy val updatedVerticesByDimension: Seq[Seq[Seq[Clump]]] = {
    // this should recursively re-clump vertices, solely on the basis of having clump-preserving neighbour bijections
    // TODO if graphs are surviving this implementation of the triple obstruction, but shouldn't be, consider also calling nauty to obtain graph automorphisms

    def oneStepUpdate(vertexClumps: Seq[Seq[Seq[Clump]]]): Seq[Seq[Seq[Clump]]] = {

      for (g <- 0 to 1; d <- 0 to depth) {
        require(vertexClumps(g)(d).size == graph(g).bigraph.rankAtDepth(d))
      }

      def vertexClump(v: Vertex) = vertexClumps(v.graph)(v.depth)(v.index)

      (for (
        ga <- (0 to 1).iterator;
        da <- (0 until depth - 1).iterator;
        ka <- (0 until graph(ga).bigraph.rankAtDepth(da)).iterator;
        gb <- (0 to 1).iterator;
        db <- (0 until depth - 1).iterator;
        kb <- (0 until graph(gb).bigraph.rankAtDepth(db)).iterator;
        if vertexClumps(ga)(da)(ka) != vertexClumps(gb)(db)(kb);
        na = rightNeighbours(Vertex(ga, da, ka));
        nb = rightNeighbours(Vertex(gb, db, kb));
        if na.size == nb.size;
        p <- Permutations.of(na.size);
        if p.zipWithIndex.forall({ case (mb, ma) => vertexClump(na(ma)) == vertexClump(nb(mb)) })
      ) yield {
        (Vertex(ga, da, ka), Vertex(gb, db, kb))
      }).toStream.headOption match {
        case None => vertexClumps
        case Some((va, vb)) => {
          val oldClump = vertexClump(vb)
          val newClump = vertexClump(va)
          val relabelledVertexClumps = vertexClumps.map(_.map(_.map({ case x if x == oldClump => newClump; case x => x })))
          oneStepUpdate(relabelledVertexClumps)
        }
      }
    }

    oneStepUpdate(verticesByDimension.value)

  }

  override def toString = s"{ $g0, $g1 }"

  def depth = g0.bigraph.depth
  def totalRank = g0.bigraph.totalRank + g1.bigraph.totalRank

  override def hashCode() = (g0, g1).hashCode
  override def equals(other: Any) = other match {
    case p: PairOfBigraphsWithDuals => p.g0 == g0 && p.g1 == g1
    case _ => false
  }

  def apply(i: Int): BigraphWithDuals
  def associativeAtPenultimateDepth_? = {
    defectsZero_?(associativityDefects.value)
  }

  def truncate: PairOfBigraphsWithDuals
  def increaseDepth: PairOfBigraphsWithDuals

  protected def defectsZero_?(defects: Seq[Seq[Int]]) = {
    defects.forall(_.forall(_ == 0))
  }

  lazy val rankPartialSums = (for (graph <- 0 to 1; d <- 0 to apply(graph).bigraph.depth) yield apply(graph).bigraph.rankAtDepth(d)).scanLeft(0)(_ + _)
  def graphLabel(graph: Int, depth: Int, index: Int): Int = {
    //    require(index < pair(graph).bigraph.rankAtDepth(depth))
    val result = rankPartialSums(graph * (g0.bigraph.depth + 1) + depth) + index
    //    println(s"graphLabel($graph, $depth, $index) = $result")
    result
  }

  lazy val nautyGraph: ColouredGraph[(Int, Int)] = {
    // FIXME this ignores edge multiplicities!
    val adjacencies = (
      for (
        graph <- 0 to 1;
        g = apply(graph);
        d <- 0 to depth;
        i <- 0 until g.bigraph.rankAtDepth(d)
      ) yield {
        (if (d % 2 == 0) {
          val j = g.dualData(d / 2)(i)
          if (i == j) {
            Seq.empty
          } else {
            Seq(graphLabel(graph, d, j))
          }
        } else {
          Seq(graphLabel(1 - graph, d, i))
        }) ++
          (if (d == g.bigraph.depth) {
            Seq.empty
          } else {
            (for (
              j <- 0 until g.bigraph.rankAtDepth(d + 1);
              if g.bigraph.inclusions(d)(j)(i) > 0
            ) yield {
              graphLabel(graph, d + 1, j)
            })
          })
      }).toIndexedSeq
    require(adjacencies.forall(_.forall(_ < totalRank)))
    val colours = for (
      graph <- 0 to 1;
      d <- 0 to depth;
      i <- 0 until apply(graph).bigraph.rankAtDepth(d)
    ) yield (graph, d)
    Graph(totalRank, adjacencies).colour(colours)
  }

}

object PairOfBigraphsWithDuals {
  private def empty(duals0: Involution, duals1: Involution) = {
    val clumps: Seq[Seq[Seq[Int]]] = Seq(
      Seq(for (i <- 0 until duals0.size) yield (i + duals0(i) / 2)),
      Seq(for (i <- 0 until duals1.size) yield duals0.size + (i + duals1(i) / 2)))

    EvenDepthPairOfBigraphsWithDuals(
      EvenDepthBigraphWithDuals(Bigraph.empty(duals0.size), Seq(duals0)),
      EvenDepthBigraphWithDuals(Bigraph.empty(duals1.size), Seq(duals1)),
      associativityDefects = Memo(Seq.empty[Seq[Int]]),
      verticesByDimension = Memo(clumps),
      triplePointConfigurations = Memo(Seq.empty[TriplePointConfiguration]))
  }

  def apply(g0: BigraphWithDuals, g1: BigraphWithDuals): PairOfBigraphsWithDuals = {
    require(g0.bigraph.depth == g1.bigraph.depth)
    val graphs = List(g0, g1)

    var evenDepthScratch: EvenDepthPairOfBigraphsWithDuals = PairOfBigraphsWithDuals.empty(g0.dualData(0), g1.dualData(0))
    var oddDepthScratch: OddDepthPairOfBigraphsWithDuals = null
    for (workingDepth <- 1 to g0.bigraph.depth) {
      //      println(s"workingDepth = $workingDepth")
      if (workingDepth % 2 == 1) {
        oddDepthScratch = evenDepthScratch.increaseDepth
        //        println(s"oddDepthScratch = $oddDepthScratch")

        for (i <- 0 until g0.bigraph.rankAtDepth(workingDepth)) {
          //          println(s" adding dual pair $i")
          oddDepthScratch = oddDepthScratch.addDualPairAtOddDepth(g0.bigraph.inclusions(workingDepth - 1)(i), g1.bigraph.inclusions(workingDepth - 1)(i)).get
          //          println(s"oddDepthScratch = $oddDepthScratch")
        }
      } else {
        evenDepthScratch = oddDepthScratch.increaseDepth
        //        println(s"evenDepthScratch = $evenDepthScratch")
        for (graph <- 0 to 1; i <- 0 until graphs(graph).bigraph.rankAtDepth(workingDepth)) {
          val j = graphs(graph).dualData(workingDepth / 2)(i)
          if (i == j) {
            //            println(s" adding a self dual vertex to graph $graph")
            evenDepthScratch = evenDepthScratch.addSelfDualVertex(graph, graphs(graph).bigraph.inclusions(workingDepth - 1)(i)).get
            //            println(s"evenDepthScratch = $evenDepthScratch")
          } else if (j == i + 1) {
            //            println(s" adding a pair of dual vertices to graph $graph")
            evenDepthScratch = evenDepthScratch.addDualPairAtEvenDepth(graph, graphs(graph).bigraph.inclusions(workingDepth - 1)(i), graphs(graph).bigraph.inclusions(workingDepth - 1)(i + 1)).get
            //            println(s"evenDepthScratch = $evenDepthScratch")
          } else if (j == i - 1) {
            // do nothing, the previous case has already added this row
          } else {
            ???
          }
        }
      }
    }
    if (g0.bigraph.depth % 2 == 0) {
      evenDepthScratch
    } else {
      oddDepthScratch
    }

  }

  object Examples {
    val Haagerup = apply(BigraphWithDuals.Examples.Haagerup, BigraphWithDuals.Examples.dualHaagerup.increaseDepth)
  }
}
case class EvenDepthPairOfBigraphsWithDuals(g0: EvenDepthBigraphWithDuals, g1: EvenDepthBigraphWithDuals, associativityDefects: Memo[Seq[Seq[Int]]], verticesByDimension: Memo[Seq[Seq[Seq[Int]]]], triplePointConfigurations: Memo[Seq[TriplePointConfiguration]]) extends PairOfBigraphsWithDuals {
  require(depth == 0 || associativityDefects.value.size == g0.bigraph.rankAtDepth(-2))
  require(depth == 0 || associativityDefects.value.head.size == g1.bigraph.rankAtDepth(-2))

  override def apply(i: Int): EvenDepthBigraphWithDuals = i match {
    case 0 => g0
    case 1 => g1
  }

  override def associativeAtPenultimateDepth_? = {
    defectsZero_?(associativityDefects.value)
  }

  override def truncate = OddDepthPairOfBigraphsWithDuals(g0.truncate, g1.truncate)
  override def increaseDepth = {
    def defects = {
      if (depth == 0) {
        Seq.fill(g0.bigraph.rankAtDepth(0))(Seq.fill(g1.bigraph.rankAtDepth(0))(0))
      } else {
        for (i <- 0 until g0.bigraph.rankAtMaximalDepth) yield {
          for (j <- 0 until g1.bigraph.rankAtMaximalDepth) yield {
            val `m*m*`: Int = {
              val inclusion0 = g0.bigraph.inclusions.last
              val inclusion1 = g1.bigraph.inclusions.last
              val duals = g1.dualData.last
              (for (k <- 0 until g0.bigraph.rankAtDepth(-2)) yield inclusion0(i)(k) * inclusion1(duals(j))(k)).sum
            }
            val `*m*m`: Int = {
              val inclusion0 = g0.bigraph.inclusions.last
              val inclusion1 = g1.bigraph.inclusions.last
              val duals = g0.dualData.last
              (for (k <- 0 until g1.bigraph.rankAtDepth(-2)) yield inclusion0(duals(i))(k) * inclusion1(j)(k)).sum
            }
            `m*m*` - `*m*m`
          }
        }
      }
    }
    def vertexClumps = updatedVerticesByDimension.map(_ :+ Seq.empty)
    OddDepthPairOfBigraphsWithDuals(g0.increaseDepth, g1.increaseDepth, Memo(defects), Memo(vertexClumps), triplePointConfigurations.map(_ ++ newTriplePointConfigurations))
  }

  private def checkDefect(graph: Int, defect: Seq[Seq[Int]]) = {
    (graph == 0 || defect.forall(_.forall(_ >= 0)))
    // TODO if a defect is large, we still need to attach lots of edges, which might be enough to push the eigenvalue up too high.
    // TODO actually, this should be part of the odometer limit function
  }

  def addSelfDualVertex(graph: Int, row: Seq[Int]): Option[EvenDepthPairOfBigraphsWithDuals] = {
    val defects = associativityDefects.map({ defects =>
      val sign = if (graph == 0) 1 else -1
      for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
        for (j <- 0 until g1.bigraph.rankAtDepth(-2)) yield {
          defects(i)(j) + sign * row(i) * row(j)
        }
      }
    })
    def vertexClumps = {
      val oldClumps = updatedVerticesByDimension
      val z = oldClumps.flatten.flatten.max + 1
      oldClumps.updated(graph, oldClumps(graph).updated(oldClumps(graph).size - 1, oldClumps(graph).last :+ z))
    }
    (apply(1 - graph).rowAllowed_?(row) && checkDefect(graph, defects.value)) option {
      graph match {
        case 0 => EvenDepthPairOfBigraphsWithDuals(g0.addSelfDualVertex(row), g1, defects, Memo(vertexClumps), triplePointConfigurations)
        case 1 => EvenDepthPairOfBigraphsWithDuals(g0, g1.addSelfDualVertex(row), defects, Memo(vertexClumps), triplePointConfigurations)
      }
    }
  }
  def addDualPairAtEvenDepth(graph: Int, row0: Seq[Int], row1: Seq[Int]): Option[EvenDepthPairOfBigraphsWithDuals] = {
    val defects = associativityDefects.map({ defects =>
      val sign = if (graph == 0) 1 else -1
      for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
        for (j <- 0 until g1.bigraph.rankAtDepth(-2)) yield {
          defects(i)(j) + sign * (row0(i) * row1(j) + row1(i) * row0(j))
        }
      }
    })
    def vertexClumps = {
      val oldClumps = updatedVerticesByDimension
      val z = oldClumps.flatten.flatten.max + 1
      oldClumps.updated(graph, oldClumps(graph).updated(depth, oldClumps(graph)(depth) ++ Seq(z, z)))
    }
    (apply(1 - graph).rowsAllowed_?(row0, row1) && checkDefect(graph, defects.value)) option {
      graph match {
        case 0 => EvenDepthPairOfBigraphsWithDuals(g0.addDualPairOfVertices(row0, row1), g1, defects, Memo(vertexClumps), triplePointConfigurations)
        case 1 => EvenDepthPairOfBigraphsWithDuals(g0, g1.addDualPairOfVertices(row0, row1), defects, Memo(vertexClumps), triplePointConfigurations)
      }
    }
  }

  def deleteSelfDualVertex(graph: Int, index: Int): EvenDepthPairOfBigraphsWithDuals = graph match {
    case 0 => EvenDepthPairOfBigraphsWithDuals(g0.deleteSelfDualVertex(index), g1)
    case 1 => EvenDepthPairOfBigraphsWithDuals(g0, g1.deleteSelfDualVertex(index))
  }
  def deleteDualPairAtEvenDepth(graph: Int, index: Int): EvenDepthPairOfBigraphsWithDuals = graph match {
    case 0 => EvenDepthPairOfBigraphsWithDuals(g0.deleteDualPairOfVertices(index), g1)
    case 1 => EvenDepthPairOfBigraphsWithDuals(g0, g1.deleteDualPairOfVertices(index))
  }
}

case class OddDepthPairOfBigraphsWithDuals(g0: OddDepthBigraphWithDuals, g1: OddDepthBigraphWithDuals, associativityDefects: Memo[Seq[Seq[Int]]], verticesByDimension: Memo[Seq[Seq[Seq[Int]]]], triplePointConfigurations: Memo[Seq[TriplePointConfiguration]]) extends PairOfBigraphsWithDuals {
  override def apply(i: Int): OddDepthBigraphWithDuals = i match {
    case 0 => g0
    case 1 => g1
  }

  override def truncate = EvenDepthPairOfBigraphsWithDuals(g0.truncate, g1.truncate)
  override def increaseDepth = {
    def defects = {
      for (i <- 0 until g0.bigraph.rankAtMaximalDepth) yield {
        for (j <- 0 until g1.bigraph.rankAtMaximalDepth) yield {
          if (depth == 0) {
            0
          } else {
            val `m*m*`: Int = {
              val inclusion = g0.bigraph.inclusions.last
              val duals = g0.dualData.last
              (for (k <- 0 until g0.bigraph.rankAtDepth(-2)) yield inclusion(i)(k) * inclusion(j)(duals(k))).sum
            }
            val `*m*m`: Int = {
              val inclusion = g1.bigraph.inclusions.last
              val duals = g1.dualData.last
              (for (k <- 0 until g1.bigraph.rankAtDepth(-2)) yield inclusion(i)(k) * inclusion(j)(duals(k))).sum
            }
            `m*m*` - `*m*m`
          }
        }
      }
    }
    def vertexClumps = updatedVerticesByDimension.map(_ :+ Seq.empty)
    EvenDepthPairOfBigraphsWithDuals(g0.increaseDepth, g1.increaseDepth, Memo(defects), Memo(vertexClumps), triplePointConfigurations.map(_ ++ newTriplePointConfigurations))
  }

  def addDualPairAtOddDepth(row0: Seq[Int], row1: Seq[Int]): Option[OddDepthPairOfBigraphsWithDuals] = {
    val allowed = {
      // this tests whether associativity between depth n-2 vertices and the new depth n pair is satisfied
      g0.bigraph.depth == 1 ||
        (for (i /* indexing vertices at the previous odd depth */ <- (0 until g0.bigraph.rankAtDepth(-3)).iterator) yield {
          val RL = (for (j <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
            g0.bigraph.inclusions.secondLast(j)(i) * row0(g0.dualData.last(j))
          }).sum
          val LR = (for (j <- 0 until g1.bigraph.rankAtDepth(-2)) yield {
            g1.bigraph.inclusions.secondLast(j)(i) * row1(g1.dualData.last(j))
          }).sum
          RL - LR
        }).forall(_ == 0)
    }
    def defects = associativityDefects.map({ oldDefects =>
      def row(i: Int) = i match {
        case 0 => row0
        case 1 => row1
      }
      for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
        for (j <- 0 until g1.bigraph.rankAtDepth(-2)) yield {
          oldDefects(i)(j) + row0(i) * row1(g1.dualData.last(j)) - row0(g0.dualData.last(i)) * row1(j)
        }
      }
    })
    def vertexClumps = {
      val oldClumps = updatedVerticesByDimension
      val z = oldClumps.flatten.flatten.max + 1
      oldClumps.map(cg => cg.updated(cg.size - 1, cg(cg.size - 1) :+ z))
    }
    allowed option
      OddDepthPairOfBigraphsWithDuals(g0.addOddVertex(row0), g1.addOddVertex(row1), associativityDefects = defects, verticesByDimension = Memo(vertexClumps), triplePointConfigurations)
  }
  def deleteDualPairAtOddDepth(index: Int) = OddDepthPairOfBigraphsWithDuals(g0.deleteOddVertex(index), g1.deleteOddVertex(index))
}

object EvenDepthPairOfBigraphsWithDuals {
  def apply(g0: EvenDepthBigraphWithDuals, g1: EvenDepthBigraphWithDuals): EvenDepthPairOfBigraphsWithDuals = {
    PairOfBigraphsWithDuals(g0, g1) match {
      case r: EvenDepthPairOfBigraphsWithDuals => r
    }
  }

  //    def defects = for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
  //      for (j <- 0 until g1.bigraph.rankAtDepth(-2)) yield {
  //        val `m*m*`: Int = {
  //          val updown: Int = {
  //            val inclusion = g0.bigraph.inclusions.last
  //            val duals = g0.dualData.last
  //            (for (k <- 0 until g0.bigraph.rankAtMaximalDepth) yield inclusion(k)(i) * inclusion(duals(k))(j)).sum
  //          }
  //          val downup: Int = {
  //            val inclusion = g0.bigraph.inclusions.secondLast
  //            val duals = g0.dualData.secondLast
  //            (for (k <- 0 until g0.bigraph.rankAtDepth(-3)) yield inclusion(i)(k) * inclusion(j)(duals(k))).sum
  //          }
  //          updown + downup
  //        }
  //        val `*m*m`: Int = {
  //          val updown: Int = {
  //            val inclusion = g1.bigraph.inclusions.last
  //            val duals = g1.dualData.last
  //            (for (k <- 0 until g1.bigraph.rankAtMaximalDepth) yield inclusion(k)(i) * inclusion(duals(k))(j)).sum
  //          }
  //          val downup: Int = {
  //            val inclusion = g1.bigraph.inclusions.secondLast
  //            val duals = g1.dualData.secondLast
  //            (for (k <- 0 until g1.bigraph.rankAtDepth(-3)) yield inclusion(i)(k) * inclusion(j)(duals(k))).sum
  //          }
  //          updown + downup
  //        }
  //        `m*m*` - `*m*m`
  //      }
  //    }
  //    EvenDepthPairOfBigraphsWithDuals(g0, g1,
  //      Memo(defects),
  //      Memo(PairOfBigraphsWithDuals.computeVerticesByDimensions(g0, g1)),
  //      Memo(PairOfBigraphsWithDuals.computeTriplePoints(g0, g1)))
}

object OddDepthPairOfBigraphsWithDuals {
  def apply(g0: OddDepthBigraphWithDuals, g1: OddDepthBigraphWithDuals): OddDepthPairOfBigraphsWithDuals = {
    PairOfBigraphsWithDuals(g0, g1) match {
      case r: OddDepthPairOfBigraphsWithDuals => r
    }
  }

  //  def apply(g0: OddDepthBigraphWithDuals, g1: OddDepthBigraphWithDuals): OddDepthPairOfBigraphsWithDuals = {
  //    def defects = {
  //      if (g0.bigraph.depth == 1 && g1.bigraph.depth == 1 && g0.bigraph.rankAtDepth(1) == 0 && g1.bigraph.rankAtDepth(1) == 0) {
  //        Seq.fill(g0.bigraph.rankAtDepth(0))(Seq.fill(g1.bigraph.rankAtDepth(0))(0))
  //      } else {
  //        for (i <- 0 until g0.bigraph.rankAtDepth(-2)) yield {
  //          for (j <- 0 until g1.bigraph.rankAtDepth(-2)) yield {
  //            val `m*m*`: Int = {
  //              val updown: Int = {
  //                val inclusion0 = g0.bigraph.inclusions.last
  //                val inclusion1 = g1.bigraph.inclusions.last
  //                val duals = g1.dualData.last
  //                (for (k <- 0 until g0.bigraph.rankAtMaximalDepth) yield inclusion0(k)(i) * inclusion1(k)(duals(j))).sum
  //              }
  //              val downup: Int = {
  //                val inclusion0 = g0.bigraph.inclusions.secondLast
  //                val inclusion1 = g1.bigraph.inclusions.secondLast
  //                val duals = g1.dualData.last
  //                (for (k <- 0 until g0.bigraph.rankAtDepth(-3)) yield inclusion0(i)(k) * inclusion1(duals(j))(k)).sum
  //              }
  //              updown + downup
  //            }
  //            val `*m*m`: Int = {
  //              val updown: Int = {
  //                val inclusion0 = g0.bigraph.inclusions.last
  //                val inclusion1 = g1.bigraph.inclusions.last
  //                val duals = g0.dualData.last
  //                (for (k <- 0 until g1.bigraph.rankAtMaximalDepth) yield inclusion0(k)(duals(i)) * inclusion1(k)(j)).sum
  //              }
  //              val downup: Int = {
  //                val inclusion0 = g0.bigraph.inclusions.secondLast
  //                val inclusion1 = g1.bigraph.inclusions.secondLast
  //                val duals = g0.dualData.last
  //                (for (k <- 0 until g1.bigraph.rankAtDepth(-3)) yield inclusion0(duals(i))(k) * inclusion1(j)(k)).sum
  //              }
  //              updown + downup
  //            }
  //            `m*m*` - `*m*m`
  //          }
  //        }
  //      }
  //    }
  //    OddDepthPairOfBigraphsWithDuals(g0, g1, Memo(defects), Memo(???), Memo(???))
  //  }
}
