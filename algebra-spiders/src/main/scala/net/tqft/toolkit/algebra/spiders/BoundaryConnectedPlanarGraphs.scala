package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.functions.Memo

object BoundaryConnectedPlanarGraphs {
  private val spider = DiagramSpider.graphSpider

  // private val IPendant = spider.multiply(spider.multiply(PlanarGraph.trivalentVertex, spider.rotate(PlanarGraph.twoSquares, -1), 2), PlanarGraph.trivalentVertex, 2)

  private def connectedGraphsImpl(r: Int, s: Int) = {
    if (r > 2) {
      ConnectedPlanarTrivalentGraphs(r, s).filterNot(_.hasTinyFace)
    } else if (r == 2 && s == 0) {
      Seq(PlanarGraph.strand)
    } else {
      Seq.empty
    }
  }

  private val connectedGraphs = {
    // "softly" keeps as much as it can, memory permitting.
    Memo.softly(connectedGraphsImpl _)
  }

  def trivalent(boundaryPoints: Int, internalFaces: Int) = {
    // Enumerate boundary-connected trivalent graphs with exactly n boundary points and k internal faces. Avoid graphs containing bigons, triangles, two adjacent squares, and a pentagon next to a square. 

    apply(boundaryPoints, internalFaces, Seq(PlanarGraph.twoSquares, PlanarGraph.pentaSquare))
  }

  private val filteredConnectedGraphs = {
    def filtered(forbiddenSubgraphs: Seq[PlanarGraph]) = {
      def graphs(r: Int, s: Int) = {
        if (r > 2) {
          connectedGraphs(r, s).par.filterNot(_.containsOneOf(forbiddenSubgraphs)).flatMap(
            (G: PlanarGraph) => Seq.tabulate(r)((rotation: Int) => DiagramSpider.graphSpider.rotate(G, rotation).canonicalFormWithDefect._1)).distinct.seq
        } else if (r == 2 && s == 0) {
          Seq(PlanarGraph.strand)
        } else {
          Seq.empty
        }
      }

      Memo.softly(graphs _)
    }
    Memo(filtered _)
  }

  private val compositions = {
    // Weak integer compositions of n into s cells
    def compositions_(n: Int, s: Int): Stream[Seq[Int]] = (1 until s).foldLeft(Stream(Nil: Seq[Int])) {
      (a, _) => a.flatMap(c => Stream.range(0, n - c.sum + 1).map(_ +: c))
    }.map(c => (n - c.sum) +: c)

    Memo.softly(compositions_ _)
  }

  def apply(n: Int, k: Int, forbiddenSubgraphs: Seq[PlanarGraph] = Seq()): Seq[PlanarGraph] = {
    // Returns Seq[PlanarGraph] containing planar graphs of n boundary points and exactly k faces.
    // The parameter connectedGraphs(r, s) gives a Seq of PlanarGraphs with r boundary points and s internal faces, which is used to 
    def product(xx: Seq[Seq[PlanarGraph]]): Seq[Seq[PlanarGraph]] =
      xx match {
        case aa +: Nil =>
          aa.map(Seq(_))
        case aa +: bb +: Nil =>
          aa.map(a => bb.map(b => Seq(a, b))).flatten
        case aa +: bb +: cc =>
          product(bb +: cc).map(li => aa.map(a => a +: li)).flatten
        case Seq() +: _ =>
          Seq()
        case _ => xx
      }

    val cachedConnectedGraphs = filteredConnectedGraphs(forbiddenSubgraphs)

    PlanarPartitions(n).flatMap((partition: Seq[Seq[Int]]) => {
      //println(s"partition: $partition")

      compositions(k, partition.length).flatMap((internalFaceNumbers: Seq[Int]) => {
        assert(internalFaceNumbers.length == partition.length, "Lengths of partition and composition do not match!")
        val components = for (i <- 0 until partition.length) yield cachedConnectedGraphs(partition(i).length, internalFaceNumbers(i))

        //println(s"components: $components")
        //for (x <- components) {println(x)}
        //println(s"product(components): ${product(components)}")
        //for (x <- product(components)) {println(x)}

        product(components).map((ds: Seq[PlanarGraph]) => { DiagramSpider.graphSpider.assembleAlongPlanarPartition(partition, ds) })
      })
    })

  }
}