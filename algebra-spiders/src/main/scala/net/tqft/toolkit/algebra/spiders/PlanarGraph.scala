package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.graphs.ColouredGraph
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import scala.collection.mutable.ListBuffer
import net.tqft.toolkit.Logging

// flags veer to the left
// edges are ordered clockwise around each vertex
case class PlanarGraph(outerFace: Int, vertexFlags: IndexedSeq[Seq[(Int, Int)]], loops: Int) { graph =>
  verify

  def verify = {
    // There are many things we might check here!

    require(vertexFlags(0).headOption match {
      case None => true
      case Some((_, f)) => f == outerFace
    })

    require(edgeSet == edgeVertexIncidences.keys.toList.sorted, s"edgeSet $edgeSet didn't match with the keys in edgeVertexIncidences: $edgeVertexIncidences")
    require(edgeSet == edgeFaceIncidences.keys.toList.sorted)

    for ((e, (v1, v2)) <- edgeVertexIncidences) {
      require(vertexFlags(v1).exists(p => p._1 == e))
      require(vertexFlags(v2).exists(p => p._1 == e))
    }

    // Compare two descriptions of edge-face incidences
    for ((e, (f1, f2)) <- edgeFaceIncidences) {
      require(faceBoundary(f1).exists(p => p._2 == e))
      require(faceBoundary(f2).exists(p => p._2 == e))
    }
    for (f <- faceSet; (v, e) <- faceBoundary(f)) {
      require(edgeFaceIncidences(e)._1 == f || edgeFaceIncidences(e)._2 == f)
    }
  }

  def numberOfBoundaryPoints = vertexFlags(0).size

  def numberOfVertices = vertexFlags.size

  def vertices = 0 until numberOfVertices
  def degree(i: Int) = vertexFlags(i).size

  lazy val edgeSet = vertexFlags.flatMap(_.map(_._1)).sorted.distinct
  lazy val faceSet = (outerFace +: vertexFlags.flatMap(_.map(_._2))).sorted.distinct

  def internalEdges = edgeSet.filterNot(boundaryEdges.contains)

  def numberOfEdges = edgeSet.size
  def numberOfFaces = faceSet.size

  def edgesAdjacentTo(vertex: Int): Seq[Int] = vertexFlags(vertex).map(_._1)
  def neighboursOf(vertex: Int) = edgesAdjacentTo(vertex).map(e => target(vertex, e))

  lazy val edgeVertexIncidences: Map[Int, (Int, Int)] = {
    val map = scala.collection.mutable.Map[Int, ListBuffer[Int]]()
    for (v <- vertices; e <- edgesAdjacentTo(v)) {
      map.getOrElseUpdate(e, ListBuffer[Int]()) += v
    }
    Map() ++ map.mapValues({ case ListBuffer(v1, v2) => (v1, v2); case _ => require(false); ??? })
  }
  lazy val edgeFaceIncidences: Map[Int, (Int, Int)] = {
    val map = scala.collection.mutable.Map[Int, ListBuffer[Int]]()
    for (flags <- vertexFlags; (e, f) <- flags) {
      map.getOrElseUpdate(e, ListBuffer[Int]()) += f
    }
    Map() ++ map.mapValues({ case ListBuffer(f1, f2) => (f1, f2); case _ => require(false); ??? })
  }
  lazy val maxEdgeLabel = edgeVertexIncidences.keysIterator.max

  lazy val maxFaceLabel = vertexFlags.iterator.flatMap(_.map(_._2)).max

  def target(source: Int, edge: Int): Int = {
    val ends = edgeVertexIncidences(edge)
    if (ends._1 == source) {
      ends._2
    } else {
      ends._1
    }
  }

  lazy val boundaryEdges = vertexFlags(0).map(_._1)
  lazy val boundaryFaces = vertexFlags(0).map(_._2)

  type EdgeFace = (Int, Int)

  def faceNeighbours(face: Int): Seq[EdgeFace] = {
    faceBoundary(face).map(p => edgeFaceIncidences(p._2) match {
      case (`face`, f) => (p._2, f); case (f, `face`) => (p._2, f)
    })
  }

  def edgesBetweenFaces(face1: Int, face2: Int) = {
    edgeFaceIncidences.collect({ case (e, (f1, f2)) if f1 == face1 && f2 == face2 || f1 == face2 && f2 == face1 => e })
  }
  def edgeBetweenFaces(face1: Int, face2: Int) = {
    edgesBetweenFaces(face1: Int, face2: Int).toList match {
      case List(e) => e
      case _ => require(false); ??? // this shouldn't happen
    }
  }

  type VertexEdge = (Int, Int)

  def faceBoundary(face: Int): Seq[VertexEdge] = {
    if (numberOfEdges == 0) {
      require(face == outerFace)
      Seq.empty
    } else {
      val initialEdge = edgeFaceIncidences.find(p => p._2._1 == face || p._2._2 == face).get._1
      val initialVertex = vertexFlags.indexWhere(_.contains((initialEdge, face)))
      def nextFlag(p: (Int, Int)): (Int, Int) = {
        val (vertex, edge) = p
        //      require(vertices.contains(vertex))
        //      require(edgeSet.contains(edge))
        val nextVertex = target(vertex, edge)
        val nextEdge = {
          import net.tqft.toolkit.collections.Rotate._
          vertexFlags(nextVertex)((vertexFlags(nextVertex).zip(vertexFlags(nextVertex).rotateLeft(1)).indexWhere(p => p._1._1 == edge && p._2._2 == face) + 1) % vertexFlags(nextVertex).size)._1
        }
        (nextVertex, nextEdge)
      }
      import net.tqft.toolkit.functions.FixedPoint._
      val (boundary, offset) = (nextFlag _).findRepeatingSubsequence((initialVertex, initialEdge))

      require(offset == 0, s"Something went wrong computing faceBoundary($face): ${Iterator.iterate((initialVertex, initialEdge))(nextFlag).take(10).toList}")

      boundary
    }
  }

  lazy val distancesFromOuterFace = distancesFromOuterFaceAvoiding(Seq.empty)

  def distancesFromOuterFaceAvoiding(edges: Seq[Int]): Map[Int, Int] = {
    def facesByRadius: Stream[Set[Int]] = {
      def internal: Stream[Set[Int]] = Set.empty[Int] #:: Set(outerFace) #:: internal.zip(internal.tail).map(p => p._2.flatMap(f => faceNeighbours(f).collect({ case (e, f) if !edges.contains(e) => f })) -- p._2 -- p._1)
      internal.tail
    }
    (for ((faces, i) <- facesByRadius.takeWhile(_.nonEmpty).iterator.zipWithIndex; f <- faces) yield (f -> i)).toMap.withDefault(_ => Integer.MAX_VALUE)
  }

  def geodesics(initialFace: Int, avoidingEdges: Seq[Int] = Seq.empty): Iterator[Seq[EdgeFace]] = {
    val distances = distancesFromOuterFaceAvoiding(avoidingEdges)

    def geodesics_(face: Int): Iterator[Seq[EdgeFace]] = {
      if (face == outerFace) {
        Iterator(Seq())
      } else {
        for ((e, f) <- faceNeighbours(face).iterator; if !avoidingEdges.contains(e); if distances(f) == distances(face) - 1; remainder <- geodesics_(f)) yield {
          (e, face) +: remainder
        }
      }
    }

    geodesics_(initialFace)
  }

  lazy val relabelEdgesAndFaces: PlanarGraph = {
    val edgeMap = (for ((e, ei) <- edgeSet.zipWithIndex) yield (e, numberOfVertices + ei)).toMap
    val faceMap = (for ((f, fi) <- faceSet.zipWithIndex) yield (f, numberOfVertices + edgeMap.size + fi)).toMap

    if (edgeMap.forall(p => p._1 == p._2) && faceMap.forall(p => p._1 == p._2)) {
      this
    } else {
      PlanarGraph(faceMap(outerFace), vertexFlags.map(_.map(p => (edgeMap(p._1), faceMap(p._2)))), loops)
    }
  }

  def nautyGraph: ColouredGraph[Int] = {
    require(edgeSet == (numberOfVertices until numberOfVertices + edgeSet.size))
    require(faceSet == (numberOfVertices + edgeSet.size until numberOfVertices + edgeSet.size + faceSet.size))

    val edgeToFaceAdjacencies = IndexedSeq.fill(edgeSet.size)(ListBuffer[Int]())

    val vertexToEdgeAdjacencies = for (flags <- vertexFlags) yield {
      flags.map(flag => {
        edgeToFaceAdjacencies(flag._1 - numberOfVertices) += flag._2
        flag._1
      })
    }
    val withExtraEdge = vertexToEdgeAdjacencies.updated(0, vertexToEdgeAdjacencies(0) :+ outerFace)

    val flagSet = for ((flags, v) <- vertexFlags.zipWithIndex; (e, f) <- flags) yield Seq(v, e, f)
    var i = numberOfVertices + edgeSet.size + faceSet.size - 1
    val flagsFore = for ((flags, v) <- vertexFlags.zipWithIndex; ((e, f), j) <- flags.zipWithIndex) yield {
      i = i + 1
      Seq(i, i + 2 * flagSet.size)
    }
    i = numberOfVertices + edgeSet.size + faceSet.size - 1
    val flagsAft = for ((flags, v) <- vertexFlags.zipWithIndex; ((e, f), j) <- flags.zipWithIndex) yield {
      i = i + 1
      if (j == 0) {
        Seq(i + flags.size - 1)
      } else {
        Seq(i - 1)
      }
    }

    ColouredGraph(numberOfVertices + edgeSet.size + faceSet.size + 3 * flagSet.size,
      withExtraEdge ++ edgeToFaceAdjacencies ++ IndexedSeq.fill(faceSet.size)(Seq.empty) ++ flagSet ++ flagsFore ++ flagsAft,
      (0 +: IndexedSeq.fill(numberOfVertices - 1)(1)) ++ IndexedSeq.fill(edgeSet.size)(2) ++ faceSet.map({ case `outerFace` => 3; case _ => 4 }) ++ IndexedSeq.fill(flagSet.size)(5) ++ IndexedSeq.fill(flagSet.size)(6) ++ IndexedSeq.fill(flagSet.size)(7))
  }

  private def cutAlong(pathOfEdgesFaces: Seq[EdgeFace]): PlanarGraph = {
    require(pathOfEdgesFaces.distinct.size == pathOfEdgesFaces.size)

    pathOfEdgesFaces match {
      case Seq() => this
      case initialPath :+ ((penultimateEdge, penultimateFace)) => {
        val oneCut = cutEdgeParallelToBoundary(penultimateEdge, penultimateFace)
        oneCut.cutAlong(initialPath)
      }
    }
  }

  def outerFaceBoundary = {
    import net.tqft.toolkit.collections.Rotate._
    val boundary = faceBoundary(outerFace)
    boundary.rotateLeft(boundary.indexWhere(p => p._1 == 0 && p._2 == vertexFlags(0)(0)._1)) // rotate the face so we start at the external vertex
  }

  private def cutEdgeParallelToBoundary(edge: Int, internalFace: Int): PlanarGraph = {
    import net.tqft.toolkit.collections.Rotate._
    val boundaryOfInnerFace = {
      val boundary = faceBoundary(internalFace)
      require(boundary.exists(_._2 == edge))
      boundary.rotateLeft(boundary.indexWhere(_._2 == edge)) // rotate the face so we start at the given edge
    }
    val (initialSegmentOfOuterFace, finalSegmentOfOuterFace) = {
      val b = outerFaceBoundary
      val i = b.indexWhere(_._2 == edge)
      (b.take(i).map(_._2), b.drop(i + 1).map(_._2))
    }

    val f = internalFace
    val o = outerFace
    val g = faceSet.max + 1
    val h = if (numberOfBoundaryPoints == 0) {
      g
    } else {
      faceSet.max + 2
    }
    val e1 = edgeSet.max + 1
    val e2 = edgeSet.max + 2

    def updateFlag(external: Boolean): ((Int, Int)) => (Int, Int) = {
      case (ee, ff) if ff == outerFace && initialSegmentOfOuterFace.contains(ee) && finalSegmentOfOuterFace.contains(ee) => (ee, if (external) h else g) // the degenerate case for 1-boundary point diagrams
      case (ee, ff) if ff == outerFace && initialSegmentOfOuterFace.contains(ee) => (ee, h)
      case (ee, ff) if ff == outerFace && finalSegmentOfOuterFace.contains(ee) => (ee, g)
      case (`edge`, `f`) => (e1, f)
      case (`edge`, `o`) => (e2, h)
      case (ee, ff) => (ee, ff)
    }

    val outerFlag = (e2, f) +: vertexFlags.head.map(updateFlag(true)) :+ (e1, g)
    val internalFlags = vertexFlags.tail.map(_.map(updateFlag(false)))

    val newVertexFlags = outerFlag +: internalFlags
    PlanarGraph(f, newVertexFlags, loops)
  }

  private def deleteSubgraph(verticesToDelete: Seq[Int], boundaryEdgesAndFacesToDelete: Seq[(Int, Int)]) = {
    require(!verticesToDelete.contains(0))
    require(verticesToDelete.forall(vertices.contains))

    val boundaryEdgesToDelete = boundaryEdgesAndFacesToDelete.map(_._1)

    require(boundaryEdgesToDelete.forall(edgeSet.contains))

    require({
      boundaryEdgesAndFacesToDelete.head._2 == outerFace
    })

    val facesAroundSubgraph: Seq[Int] = boundaryEdgesAndFacesToDelete.map(_._2)

    val perimeterEdges = {
      outerFaceBoundary.map(_._2)
    }
    val finalSegmentPerimeter = {
      import net.tqft.toolkit.collections.TakeToFirst._
      perimeterEdges.reverse.takeToFirst(e => boundaryEdgesToDelete.contains(e)).toList.reverse
    }

    val newFace = if (numberOfBoundaryPoints == 0) {
      outerFace
    } else {
      faceSet.max + 1
    }

    val updateFlag: ((Int, Int)) => (Int, Int) = {
      case (e, f) if finalSegmentPerimeter.contains(e) && f == outerFace => (e, newFace)
      case (e, f) => (e, f)
    }

    val newExternalFlag = {
      val newBoundaryEdges = boundaryEdgesToDelete.zip(newFace +: facesAroundSubgraph.tail).map(updateFlag)
      if (numberOfBoundaryPoints == 0) {
        newBoundaryEdges
      } else {
        (vertexFlags.head.head +: vertexFlags.head.tail.map(updateFlag)) ++ newBoundaryEdges
      }
    }
    val newInternalFlags = vertexFlags.zipWithIndex.tail.collect({
      case (flags, i) if !verticesToDelete.contains(i) => flags.map(updateFlag)
    })
    PlanarGraph(if (numberOfBoundaryPoints == 0) newFace else outerFace, newExternalFlag +: newInternalFlags, loops)
  }

  case class Subgraphs(shape: PlanarGraph) {
    // TODO require that every edge of shape attaches to an internal vertex?

    private def spider = implicitly[DiagramSpider[PlanarGraph]]

    private val packedShape = shape.relabelEdgesAndFaces

    case class Excision(cut: PlanarGraph, depth: Int, rotations: Rotation) {
      verify

      private def verify = {
        val result = replace(shape)
        val graphCanonicalForm = spider.canonicalFormWithDefect(graph)
        val resultCanonicalForm = spider.canonicalFormWithDefect(result)
        require(graphCanonicalForm._1 == resultCanonicalForm._1)
        Logging.info(this + " looks good to me!")
      }

      def replace(other: PlanarGraph): PlanarGraph = spider.stitchesAt(spider.rotate(spider.multiply(other, cut, spider.circumference(shape)), depth), depth, depth)
    }

    def excisions: Iterator[Excision] = {

      case class PartialMap(map: Array[Int], vertexRotations: Array[Int]) {
        override def clone = PartialMap(map.clone, vertexRotations.clone)

        import net.tqft.toolkit.arithmetic.Mod._
        lazy val edgeMap = {
          (for (
            (flags, i) <- packedShape.vertexFlags.tail.zipWithIndex;
            ((e, f), j) <- flags.zipWithIndex
          ) yield {
            e -> graph.vertexFlags(map(i))((j + vertexRotations(i)) mod graph.vertexFlags(map(i)).size)._1
          }).toMap
        }
        lazy val faceMap = {
          (for (
            (flags, i) <- packedShape.vertexFlags.tail.zipWithIndex;
            ((e, f), j) <- flags.zipWithIndex
          ) yield {
            f -> graph.vertexFlags(map(i))((j + vertexRotations(i)) mod graph.vertexFlags(map(i)).size)._2
          }).toMap
        }
        def finalVertexRotations = {
          val newMap = scala.collection.mutable.Map[Int, Int]().withDefault(_ => 0)
          for ((r, i) <- vertexRotations.zipWithIndex) {
            val d = packedShape.degree(i + 1)
            newMap(d) = newMap(d) + r
          }
          Rotation(Map() ++ newMap)
        }

        override def toString = s"PartialMap(${map.toSeq}, ${vertexRotations.toSeq})"
      }

      // map tells us where vertices/edges/faces are going. values of -1 mean they haven't been assigned yet
      def extendPartialExcision(partial: PartialMap): Iterator[Excision] = {

        def mapVertex(sourceVertex: Int, targetVertex: Int, rotation: Int, partial: PartialMap): Option[PartialMap] = {

          require(sourceVertex < shape.numberOfVertices)
          require(targetVertex < graph.numberOfVertices)
          import net.tqft.toolkit.arithmetic.Mod._
          if (partial.map(sourceVertex - 1) == targetVertex && (((partial.vertexRotations(sourceVertex - 1) - rotation) mod packedShape.degree(sourceVertex)) == 0)) {
            // already done this vertex
            Some(partial)
          } else if (targetVertex == 0 || partial.map.contains(targetVertex) || partial.map(sourceVertex - 1) != -1 || packedShape.degree(sourceVertex) != graph.degree(targetVertex)) {
            Logging.info(s"rejecting mapVertex($sourceVertex, $targetVertex, $rotation, $partial)")
            None
          } else {
            Logging.info(s"accepting mapVertex($sourceVertex, $targetVertex, $rotation, $partial)")
            partial.map(sourceVertex - 1) = targetVertex
            partial.vertexRotations(sourceVertex - 1) = rotation mod packedShape.degree(sourceVertex)

            import net.tqft.toolkit.collections.Rotate._
            val sourceNeighbours = packedShape.edgesAdjacentTo(sourceVertex).zip(packedShape.neighboursOf(sourceVertex))
            val targetNeighbours = graph.edgesAdjacentTo(targetVertex).zip(graph.neighboursOf(targetVertex)).rotateLeft(rotation)

            val next = for (((es, s), (et, t)) <- sourceNeighbours.zip(targetNeighbours); if s != 0) yield {
              val r = graph.edgesAdjacentTo(t).indexOf(et) - packedShape.edgesAdjacentTo(s).indexOf(es)
              (s, t, r)
            }

            next.foldLeft[Option[PartialMap]](Some(partial))((o, n) => o.flatMap(p => mapVertex(n._1, n._2, n._3, p)))
          }
        }

        val i = partial.map.indexOf(-1) + 1 //
        if (i == 0) {
          require(!partial.vertexRotations.contains(-1))
          // build an Excision
          // first, choose a path to cut along
          val interiorEdges = packedShape.internalEdges.map(partial.edgeMap)
          val allowedGeodesics = graph.geodesics(partial.faceMap(packedShape.outerFace), interiorEdges)

          if (allowedGeodesics.hasNext) {
            val geodesic = allowedGeodesics.next
            val verticesToDelete = partial.map
            val cut = graph.cutAlong(geodesic)

            val boundaryEdgesAndFacesToDelete_2 = for (e <- packedShape.boundaryEdges.reverse) yield {
              val i = packedShape.vertexFlags.tail.indexWhere(_.exists(_._1 == e)) // find the vertex connected to e
              require(i != -1)
              val j = packedShape.vertexFlags(i + 1).indexWhere(_._1 == e) // find which edge is e
              require(j != -1)
              val flag = cut.vertexFlags(partial.map(i))

              import net.tqft.toolkit.arithmetic.Mod._

              (i, j, flag((j + partial.vertexRotations(i)) mod flag.size))
            }

            val boundaryEdgesAndFacesToDelete = boundaryEdgesAndFacesToDelete_2.map(_._3)

            val rest = cut.deleteSubgraph(verticesToDelete, boundaryEdgesAndFacesToDelete)

            val excision = Excision(rest, geodesic.size, partial.finalVertexRotations)
            Iterator(excision)
          } else {
            Logging.info(" ... no geodesics found")
            Iterator.empty
          }

        } else if (i > numberOfVertices) {
          // this shouldn't happen?
          ???
        } else {
          // pick somewhere to send it to
          for (
            j <- (1 until graph.numberOfVertices).iterator;
            if !partial.map.contains(j);
            k <- 0 until packedShape.degree(i);
            newMap <- mapVertex(i, j, k, partial.clone).iterator;
            excision <- extendPartialExcision(newMap)
          ) yield {
            excision
          }
        }
      }

      extendPartialExcision(
        PartialMap(
          Array.fill(packedShape.numberOfVertices - 1)(-1),
          Array.fill(packedShape.numberOfVertices - 1)(-1)))
    }
  }
}

object PlanarGraph {
  private def spider = implicitly[Spider[PlanarGraph]]

  def empty = polygon(0)

  def loop = {
    PlanarGraph(0, IndexedSeq(IndexedSeq.empty), 1)
  }

  def strand = {
    PlanarGraph(0, IndexedSeq(IndexedSeq((0, 0), (0, 1))), 0)
  }

  def polygon(k: Int) = {
    if (k == 0) {
      loop
    } else {
      import net.tqft.toolkit.arithmetic.Mod._
      val flags = IndexedSeq.tabulate(k)(i => (i + k + 1, i + 3 * k + 1)) +:
        IndexedSeq.tabulate(k)(i => IndexedSeq((i + 2 * k + 1, 4 * k + 1), (i + k + 1, (i + 1 mod k) + 3 * k + 1), ((i - 1 mod k) + 2 * k + 1, i + 3 * k + 1)))
      PlanarGraph(3 * k + 1, flags, 0)
    }
  }

  def star(k: Int) = {
    val flags = IndexedSeq(
      Seq.tabulate(k)(i => (i + 2, i + k + 2)),
      Seq.tabulate(k)(i => (i + 2, ((i + 1) % k) + k + 2)).reverse)
    PlanarGraph(k + 2, flags, 0)
  }

  val I = spider.multiply(spider.rotate(star(3), 1), spider.rotate(star(3), -1), 1)
  val H = spider.rotate(I, 1)

  val theta = spider.multiply(star(3), star(3), 3)
  val tetrahedron = spider.multiply(star(3), polygon(3), 3)
  val cube = spider.multiply(polygon(4), polygon(4), 4)

  lazy val dodecahedron = {
    val penta5fork = {
      def f(g: PlanarGraph) = spider.rotate(spider.multiply(star(3), g, 1), 1)
      f(f(f(f(f(polygon(5))))))
    }
    spider.multiply(penta5fork, spider.rotate(penta5fork, 1), 10)
  }

}