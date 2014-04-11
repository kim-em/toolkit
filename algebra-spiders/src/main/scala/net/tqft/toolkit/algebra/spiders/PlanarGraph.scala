package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.graphs.ColouredGraph
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import scala.collection.mutable.ListBuffer
import net.tqft.toolkit.Logging

// flags veer to the left
// edges are ordered clockwise around each vertex
case class PlanarGraph(outerFace: Int, vertexFlags: IndexedSeq[Seq[(Int, Int)]], labels: Seq[Int], loops: Int) { graph =>
  verify

  def verify = {
    // There are many things we might check here!
    require(loops >= 0)
    
    require(labels.size == numberOfVertices - 1)

    require(vertexFlags(0).headOption match {
      case None => true
      case Some((_, f)) => f == outerFace
    })

    require(edgeSet == edgeVertexIncidences.keys.toSeq.sorted, s"edgeSet $edgeSet didn't match with the keys in edgeVertexIncidences: $edgeVertexIncidences")
    require(edgeSet == edgeFaceIncidences.keys.toSeq.sorted)

    for ((flags, i) <- vertexFlags.zipWithIndex; Seq((e1, _), (_, f2)) <- (flags ++ flags).sliding(2); t = target(i, e1); if i != t) {
      require(f2 == vertexFlags(t).find(_._1 == e1).get._2)
    }

    require(faceEdgeIncidences.keySet.contains(outerFace))
    require(edgeSet.isEmpty || faceEdgeIncidences(outerFace).nonEmpty)

    for ((e, (v1, v2)) <- edgeVertexIncidences) {
      require(vertexFlags(v1).exists(p => p._1 == e))
      require(vertexFlags(v2).exists(p => p._1 == e))
    }

    // Compare two descriptions of edge-face incidences
    for ((e, (f1, f2)) <- edgeFaceIncidences) {
      require(faceBoundary(f1).flatten.exists(p => p._2 == e))
      require(faceBoundary(f2).flatten.exists(p => p._2 == e))
    }
    for (f <- faceSet; component <- faceBoundary(f); (v, e) <- component) {
      require(edgeFaceIncidences(e)._1 == f || edgeFaceIncidences(e)._2 == f)
    }

    require(vertexFlags.head.distinct.size == vertexFlags.head.size)
  }

  def numberOfBoundaryPoints = vertexFlags(0).size

  def numberOfVertices = vertexFlags.size

  def vertices = 0 until numberOfVertices
  def degree(i: Int) = vertexFlags(i).size

  lazy val edgeSet = vertexFlags.flatMap(_.map(_._1)).sorted.distinct
  lazy val faceSet = (outerFace +: vertexFlags.flatMap(_.map(_._2))).sorted.distinct
  def internalFaceSet = faceSet.filterNot(vertexFlags.head.map(_._2).contains)

  def internalEdges = edgeSet.filterNot(boundaryEdges.contains)

  def numberOfEdges = edgeSet.size
  def numberOfFaces = faceSet.size
  def numberOfInternalFaces = numberOfFaces - boundaryFaces.distinct.size

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
  lazy val faceEdgeIncidences: Map[Int, Set[Int]] = {
    val map = scala.collection.mutable.Map[Int, ListBuffer[Int]]()
    for (flags <- vertexFlags; (e, f) <- flags) {
      map.getOrElseUpdate(f, ListBuffer[Int]()) += e
    }
    map.getOrElseUpdate(outerFace, ListBuffer[Int]())
    Map() ++ map.mapValues(_.toSet)
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

  def faceNeighbours(face: Int): Seq[Seq[EdgeFace]] = {
    faceBoundary(face).map(_.map(p => edgeFaceIncidences(p._2) match {
      case (`face`, f) => (p._2, f); case (f, `face`) => (p._2, f)
    }))
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

  lazy val faceBoundary = {
    import net.tqft.toolkit.functions.Memo._
    (faceBoundary_ _).memo
  }

  private def faceBoundary_(face: Int): Seq[Seq[VertexEdge]] = {
    if (numberOfEdges == 0) {
      require(face == outerFace)
      Seq.empty
    } else {
      val components = scala.collection.mutable.ListBuffer[Seq[VertexEdge]]()
      val edgesLocated = scala.collection.mutable.Set[Int]()
      val allEdgesAroundFace = faceEdgeIncidences(face)

      var k = 0
      while ((allEdgesAroundFace -- edgesLocated).nonEmpty) {
        val initialEdge = edgeFaceIncidences.find(p => !edgesLocated.contains(p._1) && (p._2._1 == face || p._2._2 == face)).get._1
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
        components += boundary
        edgesLocated ++= boundary.map(_._2)
        k = k + 1
        require(k < 10)

      }
      components.toList
    }
  }

  lazy val distancesFromOuterFace = distancesFromOuterFaceAvoiding(Seq.empty)

  def distancesFromOuterFaceAvoiding(edges: Seq[Int]): Map[Int, Int] = {
    def facesByRadius: Stream[Set[Int]] = {
      def internal: Stream[Set[Int]] = {
        Set.empty[Int] #::
          Set(outerFace) #::
          internal.zip(internal.tail).map({ p =>
            p._2.flatMap(f => faceNeighbours(f).flatten.collect({ case (e, f) if !edges.contains(e) => f })) -- p._2 -- p._1
          })
      }
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
        for (component <- faceNeighbours(face).iterator; (e, f) <- component; if !avoidingEdges.contains(e); if distances(f) == distances(face) - 1; remainder <- geodesics_(f)) yield {
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
      PlanarGraph(faceMap(outerFace), vertexFlags.map(_.map(p => (edgeMap(p._1), faceMap(p._2)))), labels, loops)
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
    //    val withExtraEdge = vertexToEdgeAdjacencies.updated(0, vertexToEdgeAdjacencies(0) :+ outerFace)

    val breakRotationalSymmetry = if (numberOfBoundaryPoints == 0) {
      IndexedSeq.empty
    } else {
      IndexedSeq(0, vertexFlags(0)(0)._1, vertexFlags(0)(0)._2)
    }

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

    ColouredGraph(numberOfVertices + edgeSet.size + faceSet.size + 3 * flagSet.size + 1,
      vertexToEdgeAdjacencies ++ edgeToFaceAdjacencies ++ IndexedSeq.fill(faceSet.size)(Seq.empty) ++ flagSet ++ flagsFore ++ flagsAft :+ breakRotationalSymmetry,
      (0 +: IndexedSeq.fill(numberOfVertices - 1)(1)) ++ IndexedSeq.fill(edgeSet.size)(2) ++ faceSet.map({ case `outerFace` => 3; case _ => 4 }) ++ IndexedSeq.fill(flagSet.size)(5) ++ IndexedSeq.fill(flagSet.size)(6) ++ IndexedSeq.fill(flagSet.size)(7) :+ 8)
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

  lazy val outerFaceBoundary: Seq[VertexEdge] = {
    import net.tqft.toolkit.collections.Rotate._
    val outerBoundary = faceBoundary(outerFace).flatten
    val k = outerBoundary.indexWhere(p => p._1 == 0 && p._2 == vertexFlags(0)(0)._1)
    if (k != -1) {
      outerBoundary.rotateLeft(k) // rotate the face so we start at the external vertex
    } else {
      outerBoundary

    }
  }

  private def cutEdgeParallelToBoundary(edge: Int, internalFace: Int): PlanarGraph = {
    import net.tqft.toolkit.collections.Rotate._
    val boundaryOfInnerFace = {
      val boundary = faceBoundary(internalFace).find(_.exists(_._2 == edge)).get
      boundary.rotateLeft(boundary.indexWhere(_._2 == edge)) // rotate the face so we start at the given edge
    }
    val (initialSegmentOfOuterFace, finalSegmentOfOuterFace) = {
      val b = outerFaceBoundary
      val i = b.indexWhere(_._2 == edge)
      (b.take(i), b.drop(i + 1))
    }

    val f = internalFace
    val o = outerFace
    val g = faceSet.max + 1

    def needTwoNewFaces = {
      (numberOfBoundaryPoints != 0) && faceBoundary(outerFace).find(_.exists(_._1 == 0)).get.map(_._2).contains(edge)
    }

    val h = if (needTwoNewFaces) {
      faceSet.max + 2
    } else {
      g
    }
    val e1 = edgeSet.max + 1
    val e2 = edgeSet.max + 2

    def updateFlag(i: Int, external: Boolean): ((Int, Int)) => (Int, Int) = {
      case (ee, ff) if ff == outerFace && initialSegmentOfOuterFace.contains((i, ee)) && finalSegmentOfOuterFace.contains(ee) => (ee, if (external) h else g) // the degenerate case for 1-boundary point diagrams
      case (ee, ff) if ff == outerFace && initialSegmentOfOuterFace.contains((i, ee)) => (ee, h)
      case (ee, ff) if ff == outerFace && finalSegmentOfOuterFace.contains((i, ee)) => (ee, g)
      case (`edge`, `f`) => (e1, f)
      case (`edge`, `o`) => (e2, h)
      case (ee, ff) => (ee, ff)
    }

    val outerFlag = (e2, f) +: vertexFlags.head.map(updateFlag(0, true)) :+ (e1, g)
    val internalFlags = vertexFlags.zipWithIndex.tail.map(p => p._1.map(updateFlag(p._2, false)))

    val newVertexFlags = outerFlag +: internalFlags
    PlanarGraph(f, newVertexFlags, labels, loops)
  }

  private def deleteSubgraph(verticesToDelete: Seq[Int], boundaryEdgesAndFacesToDelete: Seq[(Int, Int)], loopsToDelete: Int) = {
    require(!verticesToDelete.contains(0))
    require(verticesToDelete.forall(vertices.contains))

    if (verticesToDelete.isEmpty) {
      if (loopsToDelete > 0) {
        this.copy(loops = loops - loopsToDelete)
      } else {
        this
      }
    } else {

      val boundaryEdgesToDelete = boundaryEdgesAndFacesToDelete.map(_._1)

      require(boundaryEdgesToDelete.forall(edgeSet.contains))

      require({
        boundaryEdgesAndFacesToDelete.head._2 == outerFace
      })

      val facesAroundSubgraph: Seq[Int] = boundaryEdgesAndFacesToDelete.map(_._2)

      val finalSegmentPerimeter_old = {
        import net.tqft.toolkit.collections.TakeToFirst._
        outerFaceBoundary.reverse.takeToFirst(p => boundaryEdgesToDelete.contains(p._2)).toList.reverse
      }

      val finalSegmentPerimeter = {
        import net.tqft.toolkit.collections.TakeToFirst._
        outerFaceBoundary.reverse.takeToFirst({ p =>
          boundaryEdgesToDelete.head == p._2 && (boundaryEdgesToDelete.count(_ == p._2) == 2 || !verticesToDelete.contains(target(p._1, p._2)))
        }).toList.reverse
      }

      //      require(finalSegmentPerimeter == finalSegmentPerimeter_old)

      def needNewFace = {
        (numberOfBoundaryPoints != 0) && faceBoundary(outerFace).find(_.exists(_._1 == 0)).get.map(_._2).toSet.intersect(boundaryEdgesToDelete.toSet).nonEmpty
      }

      val newFace = if (needNewFace) {
        faceSet.max + 1
      } else {
        outerFace
      }

      def updateFlag(i: Int, force: Boolean): ((Int, Int)) => (Int, Int) = {
        case (e, f) if (finalSegmentPerimeter.contains((i, e)) || (force && finalSegmentPerimeter.exists(_._2 == e))) && f == outerFace => (e, newFace)
        case (e, f) => (e, f)
      }

      val newExternalFlag = {
        val newBoundaryEdges = boundaryEdgesToDelete.zip(newFace +: facesAroundSubgraph.tail).map(updateFlag(0, true))
        if (numberOfBoundaryPoints == 0) {
          newBoundaryEdges
        } else {
          (vertexFlags.head.head +: vertexFlags.head.tail.map(updateFlag(0, false))) ++ newBoundaryEdges
        }
      }
      val newInternalFlags = vertexFlags.zipWithIndex.tail.collect({
        case (flags, i) if !verticesToDelete.contains(i) => flags.map(updateFlag(i, false))
      })
      val newLabels = (0 +: labels).zipWithIndex.collect({ case (l, i) if !verticesToDelete.contains(i) => l }).tail
      PlanarGraph(if (numberOfBoundaryPoints == 0) newFace else outerFace, newExternalFlag +: newInternalFlags, newLabels, loops - loopsToDelete)
    }
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
        if (graphCanonicalForm._1 != resultCanonicalForm._1) {
          val step0 = spider.tensor(shape, cut)
          val step1 = spider.multiply(shape, cut, spider.circumference(shape))
          val step2 = spider.rotate(step1, depth)
          require(false)
        }
        //        Logging.info(this + " looks good to me!")
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
          if (packedShape.numberOfEdges == 0) {
            Map(packedShape.outerFace -> graph.outerFace)
          } else {
            (for (
              (flags, i) <- packedShape.vertexFlags.tail.zipWithIndex;
              ((e, f), j) <- flags.zipWithIndex
            ) yield {
              f -> graph.vertexFlags(map(i))((j + vertexRotations(i)) mod graph.vertexFlags(map(i)).size)._2
            }).toMap
          }
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
            //            Logging.info(s"rejecting mapVertex($sourceVertex, $targetVertex, $rotation, $partial)")
            None
          } else {
            //            Logging.info(s"accepting mapVertex($sourceVertex, $targetVertex, $rotation, $partial)")
            partial.map(sourceVertex - 1) = targetVertex
            partial.vertexRotations(sourceVertex - 1) = rotation mod packedShape.degree(sourceVertex)

            import net.tqft.toolkit.collections.Rotate._
            val sourceNeighbours = packedShape.edgesAdjacentTo(sourceVertex).zip(packedShape.neighboursOf(sourceVertex))
            val targetNeighbours = graph.edgesAdjacentTo(targetVertex).zip(graph.neighboursOf(targetVertex)).rotateLeft(rotation)

            val next = for (((es, s), (et, t)) <- sourceNeighbours.zip(targetNeighbours); if s != 0) yield {
              val r = if (s == sourceVertex) {
                rotation
              } else {
                graph.edgesAdjacentTo(t).indexOf(et) - packedShape.edgesAdjacentTo(s).indexOf(es)
              }
              (s, t, r)
            }

            next.foldLeft[Option[PartialMap]](Some(partial))((o, n) => o.flatMap(p => mapVertex(n._1, n._2, n._3, p)))
          }
        }

        if (packedShape.loops <= graph.loops) {

          val i = partial.map.indexOf(-1) + 1 //
          if (i == 0) {
            require(!partial.vertexRotations.contains(-1))

            // make sure we didn't swallow up anything smaller
            if (packedShape.internalFaceSet.forall(f => packedShape.faceBoundary(f).size == graph.faceBoundary(partial.faceMap(f)).size)) {

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

                val rest = cut.deleteSubgraph(verticesToDelete, boundaryEdgesAndFacesToDelete, packedShape.loops)

                val excision = Excision(rest, geodesic.size, partial.finalVertexRotations)
                Iterator(excision)
              } else {
                //              Logging.info(" ... no geodesics found")
                Iterator.empty
              }
            } else {
              // ooops, swallowed something up, ignore this one
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
        } else {
          Iterator.empty
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

  val empty = {
    PlanarGraph(1, IndexedSeq(IndexedSeq.empty), IndexedSeq.empty, 0)
  }

  val loop = {
    PlanarGraph(1, IndexedSeq(IndexedSeq.empty), IndexedSeq.empty, 1)
  }

  val strand = {
    PlanarGraph(0, IndexedSeq(IndexedSeq((0, 0), (0, 1))), IndexedSeq.empty, 0)
  }

  val two_strands_horizontal = spider.tensor(strand, strand)
  val two_strands_vertical = spider.rotate(two_strands_horizontal, 1)

  private def polygon_(k: Int) = {
    if (k == 0) {
      loop
    } else {
      import net.tqft.toolkit.arithmetic.Mod._
      val flags = IndexedSeq.tabulate(k)(i => (i + k + 1, i + 3 * k + 1)) +:
        IndexedSeq.tabulate(k)(i => IndexedSeq((i + 2 * k + 1, 4 * k + 1), (i + k + 1, (i + 1 mod k) + 3 * k + 1), ((i - 1 mod k) + 2 * k + 1, i + 3 * k + 1)))
      PlanarGraph(3 * k + 1, flags, IndexedSeq.fill(k)(1), 0)
    }
  }

  val polygon = {
    import net.tqft.toolkit.functions.Memo._
    (polygon_ _).memo
  }

  private def star_(k: Int) = {
    val flags = IndexedSeq(
      Seq.tabulate(k)(i => (i + 2, i + k + 2)),
      Seq.tabulate(k)(i => (i + 2, ((i + 1) % k) + k + 2)).reverse)
    PlanarGraph(k + 2, flags, IndexedSeq(1), 0)
  }

  val star = {
    import net.tqft.toolkit.functions.Memo._
    (star_ _).memo
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