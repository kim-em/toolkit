package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.graphs.ColouredGraph
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import scala.collection.mutable.ListBuffer

// flags veer to the left
// edges are ordered clockwise around each vertex
case class PlanarGraph(vertexFlags: IndexedSeq[Seq[(Int, Int)]], loops: Int) { graph =>
  verify

  def verify = {
    // There are many things we might check here!

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
  def outerFace = vertexFlags(0).headOption.map(_._2).getOrElse(0)

  def numberOfVertices = vertexFlags.size

  def vertices = 0 until numberOfVertices
  def degree(i: Int) = vertexFlags(i).size

  lazy val edgeSet = vertexFlags.flatMap(_.map(_._1)).sorted.distinct
  lazy val faceSet = vertexFlags.flatMap(_.map(_._2)).sorted.distinct
  def numberOfEdges = edgeSet.size
  def numberOfFaces = faceSet.size

  def edgesAdjacentTo(vertex: Int): Seq[Int] = vertexFlags(vertex).map(_._1)
  def neighboursOf(vertex: Int) = edgesAdjacentTo(vertex).map(e => target(vertex, e))

  lazy val edgeVertexIncidences: Map[Int, (Int, Int)] = {
    val map = scala.collection.mutable.Map[Int, ListBuffer[Int]]()
    for (v <- vertices; e <- edgesAdjacentTo(v)) {
      map.getOrElseUpdate(e, ListBuffer[Int]()) += v
    }
    Map() ++ map.mapValues({ case ListBuffer(v1, v2) => (v1, v2) })
  }
  lazy val edgeFaceIncidences: Map[Int, (Int, Int)] = {
    val map = scala.collection.mutable.Map[Int, ListBuffer[Int]]()
    for (flags <- vertexFlags; (e, f) <- flags) {
      map.getOrElseUpdate(e, ListBuffer[Int]()) += f
    }
    Map() ++ map.mapValues({ case ListBuffer(f1, f2) => (f1, f2) })
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

  def faceNeighbours(face: Int) = {
    for (i <- vertices; (e, `face`) <- vertexFlags(i)) yield vertexFlags(target(i, e)).find(_._1 == e).get._2
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

  def faceBoundary(face: Int): Seq[(Int, Int)] = {
    val initialEdge = edgeFaceIncidences.find(p => p._2._1 == face || p._2._2 == face).get._1
    val initialVertex = vertexFlags.indexWhere(_.contains((initialEdge, face)))
    def nextFlag(p: (Int, Int)): (Int, Int) = {
      val (vertex, edge) = p
      //      require(vertices.contains(vertex))
      //      require(edgeSet.contains(edge))
      val nextVertex = target(vertex, edge)
      val nextEdge = vertexFlags(nextVertex)((vertexFlags(nextVertex).indexWhere(_._1 == edge) + 1) % vertexFlags(nextVertex).size)._1
      (nextVertex, nextEdge)
    }
    import net.tqft.toolkit.functions.FixedPoint._
    val (boundary, offset) = (nextFlag _).findRepeatingSubsequence((initialVertex, initialEdge))

    require(offset == 0)

    boundary
  }

  lazy val distancesFromOuterFace: Map[Int, Int] = {
    def facesByRadius: Stream[Set[Int]] = {
      def internal: Stream[Set[Int]] = Set.empty[Int] #:: Set(outerFace) #:: internal.zip(internal.tail).map(p => p._2.flatMap(faceNeighbours) -- p._2 -- p._1)
      internal.tail
    }
    (for ((faces, i) <- facesByRadius.takeWhile(_.nonEmpty).iterator.zipWithIndex; f <- faces) yield (f -> i)).toMap
  }
  def geodesicsToOuterFace(face: Int): Iterator[Seq[Int]] = {
    if (face == outerFace) {
      Iterator(Seq(face))
    } else {
      for (next <- faceNeighbours(face).iterator; if distancesFromOuterFace(next) == distancesFromOuterFace(face) - 1; remainder <- geodesicsToOuterFace(next)) yield {
        face +: remainder
      }
    }
  }

  lazy val relabelEdgesAndFaces: PlanarGraph = {
    val edgeMap = (for ((e, ei) <- edgeSet.zipWithIndex) yield (e, numberOfVertices + ei)).toMap
    val faceMap = (for ((f, fi) <- faceSet.zipWithIndex) yield (f, numberOfVertices + edgeMap.size + fi)).toMap

    if (edgeMap.forall(p => p._1 == p._2) && faceMap.forall(p => p._1 == p._2)) {
      this
    } else {
      PlanarGraph(vertexFlags.map(_.map(p => (edgeMap(p._1), faceMap(p._2)))), loops)
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
      (0 +: IndexedSeq.fill(numberOfVertices - 1)(1)) ++ IndexedSeq.fill(edgeSet.size)(2) ++ IndexedSeq.fill(faceSet.size)(3) ++ IndexedSeq.fill(flagSet.size)(4) ++ IndexedSeq.fill(flagSet.size)(5) ++ IndexedSeq.fill(flagSet.size)(6))
  }

  private def cutAlong(pathOfFaces: Seq[Int]): PlanarGraph = {
    require(pathOfFaces.distinct.size == pathOfFaces.size)

    pathOfFaces match {
      case Seq(_outerFace) if _outerFace == outerFace => this
      case initialPath :+ penultimateFace :+ _outerFace if _outerFace == outerFace => {
        val oneCut = cutEdgeParallelToBoundary(edgeBetweenFaces(penultimateFace, outerFace), penultimateFace)
        oneCut.cutAlong(initialPath :+ penultimateFace)
      }
    }
  }
  private def cutEdgeParallelToBoundary(edge: Int, internalFace: Int): PlanarGraph = {
    import net.tqft.toolkit.collections.Rotate._
    val boundaryOfInnerFace = {
      val boundary = faceBoundary(internalFace)
      require(boundary.exists(_._2 == edge))
      boundary.rotateLeft(boundary.indexWhere(_._2 == edge)) // rotate the face so we start at the given edge
    }
    val boundaryOfOuterFace = {
      val boundary = faceBoundary(outerFace)
      boundary.rotateLeft(boundary.indexWhere(_._1 == 0)) // rotate the face so we start at the external vertex
    }
    val (initialSegmentOfOuterFace, finalSegmentOfOuterFace) = {
      val i = boundaryOfOuterFace.indexWhere(_._2 == edge)
      (boundaryOfOuterFace.take(i).map(_._2), boundaryOfOuterFace.drop(i + 1).map(_._2))
    }

    val f = internalFace
    val o = outerFace
    val g = faceSet.max + 1
    val h = faceSet.max + 2
    val e1 = edgeSet.max + 1
    val e2 = edgeSet.max + 2

    val updateFlag: ((Int, Int)) => (Int, Int) = {
      case (ee, ff) if ff == outerFace && initialSegmentOfOuterFace.contains(ee) => (ee, h)
      case (ee, ff) if ff == outerFace && finalSegmentOfOuterFace.contains(ee) => (ee, g)
      case (`edge`, `f`) => (e1, f)
      case (`edge`, `o`) => (e2, h)
      case (ee, ff) => (ee, ff)
    }

    val outerFlag = (e2, f) +: vertexFlags.head.map(updateFlag) :+ (e1, g)
    val internalFlags = vertexFlags.tail.map(_.map(updateFlag))

    val newVertexFlags = outerFlag +: internalFlags
    PlanarGraph(newVertexFlags, loops)
  }

  private def deleteSubgraph(verticesToDelete: Seq[Int], boundaryEdgesToDelete: Seq[Int]) = {
    require(verticesToDelete.forall(vertices.contains))
    require(boundaryEdgesToDelete.forall(edgeSet.contains))
    
    require({
      val facesThatMustBeOuterFaces = for (v <- verticesToDelete; (e, f) <- vertexFlags(v); if e == boundaryEdgesToDelete.head) yield f
      facesThatMustBeOuterFaces.size == 1 && facesThatMustBeOuterFaces.head == outerFace
    })

    val facesAroundSubgraph: Seq[Int] = for(b <- boundaryEdgesToDelete; v <- verticesToDelete; (`b`, f) <- vertexFlags(v)) yield f
    
    val perimeterEdges = faceBoundary(outerFace).map(_._2)
    val initialSegmentPerimeter = perimeterEdges.takeWhile(e => !boundaryEdgesToDelete.contains(e))

    val newFace = faceSet.max + 1
    val newExternalFlag = vertexFlags.head ++ boundaryEdgesToDelete.reverse.zip(newFace +: facesAroundSubgraph.tail) 
    val newInternalFlags = vertexFlags.zipWithIndex.tail.collect({
      case (flags, i) if !verticesToDelete.contains(i) => flags.map({
        case (e, f) if initialSegmentPerimeter.contains(e) && f == outerFace => (e, newFace)
        case (e, f) => (e, f)
      })
    })
    PlanarGraph(newExternalFlag +: newInternalFlags, loops)
  }

  case class Subgraphs(shape: PlanarGraph) {
    // TODO require that every edge of shape attaches to an internal vertex?

    private def spider = implicitly[DiagramSpider[PlanarGraph]]

    private val packedShape = shape.relabelEdgesAndFaces

    case class Excision(cut: PlanarGraph, depth: Int, rotations: Rotation) {
      require(verify)

      private def verify = {
        val result = replace(shape)
        spider.canonicalFormWithDefect(graph)._1 == spider.canonicalFormWithDefect(result)._1
      }

      def replace(other: PlanarGraph): PlanarGraph = spider.stitchesAt(spider.rotate(spider.multiply(spider.rotate(cut, depth), other, spider.circumference(shape)), -depth), depth, depth)
    }

    def excisions: Iterator[Excision] = {

      case class PartialMap(map: Array[Int], vertexRotations: Array[Int]) {
        override def clone = PartialMap(map.clone, vertexRotations.clone)
        lazy val edgeMap = {
          (for (
            (flags, i) <- packedShape.vertexFlags.tail.zipWithIndex;
            ((e, f), j) <- flags.zipWithIndex
          ) yield {
            e -> graph.vertexFlags(map(i))((j + vertexRotations(i)) % graph.vertexFlags(map(i)).size)._1
          }).toMap
        }
        lazy val faceMap = {
          (for (
            (flags, i) <- packedShape.vertexFlags.tail.zipWithIndex;
            ((e, f), j) <- flags.zipWithIndex
          ) yield {
            f -> graph.vertexFlags(map(i))((j + vertexRotations(i)) % graph.vertexFlags(map(i)).size)._2
          }).toMap
        }
      }

      // map tells us where vertices/edges/faces are going. values of -1 mean they haven't been assigned yet
      def extendPartialExcision(partial: PartialMap): Iterator[Excision] = {

        def mapVertex(sourceVertex: Int, targetVertex: Int, rotation: Int, partial: PartialMap): Option[PartialMap] = {
          if (partial.map(sourceVertex - 1) == targetVertex && partial.vertexRotations(sourceVertex - 1) == rotation) {
            // already done this vertex
            Some(partial)
          } else if (partial.map(sourceVertex - 1) != -1) {
            None
          } else if (packedShape.degree(sourceVertex) != graph.degree(targetVertex)) {
            None
          } else {
            partial.map(sourceVertex - 1) = targetVertex
            partial.vertexRotations(sourceVertex - 1) = rotation

            import net.tqft.toolkit.collections.Rotate._
            val sourceNeighbours = packedShape.edgesAdjacentTo(sourceVertex).zip(packedShape.neighboursOf(sourceVertex))
            val targetNeighbours = graph.edgesAdjacentTo(targetVertex).zip(graph.neighboursOf(targetVertex)).rotateLeft(rotation)

            val next = for (((es, s), (et, t)) <- sourceNeighbours.zip(targetNeighbours); if s != 0) yield {
              val r = packedShape.edgesAdjacentTo(s).indexOf(es) - graph.edgesAdjacentTo(t).indexOf(et)
              (s, t, r)
            }

            next.foldLeft[Option[PartialMap]](Some(partial))((o, n) => o.flatMap(p => mapVertex(n._1, n._2, n._3, p)))
          }
        }

        val i = partial.map.indexOf(-1) + 1 //
        if (i == 0) {
          // build an Excision
          // first, choose a path to cut along
          val geodesic = graph.geodesicsToOuterFace(partial.faceMap(packedShape.outerFace)).next
          val verticesToDelete = partial.map
          val cut = graph.cutAlong(geodesic)
          
          val boundaryEdgesToDelete = for(
              (flag, i) <- packedShape.vertexFlags.tail.zipWithIndex;
              ((e, f), j) <- flag.zipWithIndex;
              if packedShape.boundaryEdges.contains(e)) yield {
            cut.vertexFlags(partial.map(i))(j)._1
          }
             
          val rest = cut.deleteSubgraph(verticesToDelete, boundaryEdgesToDelete)

          val excision = Excision(rest, geodesic.size - 1, ???)
          Iterator(excision)
        } else if (i > numberOfVertices) {
          // this shouldn't happen?
          ???
        } else {
          // pick somewhere to send it to
          for (
            j <- (1 to graph.numberOfVertices).iterator;
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
          Array.fill(packedShape.numberOfVertices - 1 /* + packedShape.numberOfEdges + packedShape.numberOfFaces */ )(-1),
          Array.fill(packedShape.numberOfVertices - 1)(-1)))
    }
  }
}

object PlanarGraph {
  private def spider = implicitly[Spider[PlanarGraph]]

  def empty = polygon(0)

  def loop = {
    PlanarGraph(IndexedSeq(IndexedSeq.empty), 1)
  }

  def strand = {
    PlanarGraph(IndexedSeq(IndexedSeq((0, 0), (0, 1))), 0)
  }

  def polygon(k: Int) = {
    if (k == 0) {
      loop
    } else {
      import net.tqft.toolkit.arithmetic.Mod._
      val flags = IndexedSeq.tabulate(k)(i => (i + k + 1, i + 3 * k + 1)) +:
        IndexedSeq.tabulate(k)(i => IndexedSeq((i + 2 * k + 1, 4 * k + 1), (i + k + 1, (i + 1 mod k) + 3 * k + 1), ((i - 1 mod k) + 2 * k + 1, i + 3 * k + 1)))
      PlanarGraph(flags, 0)
    }
  }

  def star(k: Int) = {
    val flags = IndexedSeq(
      Seq.tabulate(k)(i => (i + 2, i + k + 2)),
      Seq.tabulate(k)(i => (i + 2, ((i + 1) % k) + k + 2)).reverse)
    PlanarGraph(flags, 0)
  }

  val I = spider.multiply(spider.rotate(star(3), 1), spider.rotate(star(3), -1), 1)
  val H = spider.rotate(I, 1)

}