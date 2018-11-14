package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.graphs.ColouredGraph
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import scala.collection.mutable.ListBuffer
import net.tqft.toolkit.Logging
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.JavaTokenParsers

// flags veer to the left
// edges are ordered clockwise around each vertex
case class PlanarGraph(outerFace: Int, vertexFlags: IndexedSeq[Seq[(Int, Int)]], labels: Seq[(Int, Int)], loops: Int, comment: Option[String] = None) { graph =>
  //  verify

  override lazy val hashCode = (outerFace, vertexFlags, labels, loops).hashCode

  override def equals(other: Any) = {
    other match {
      case other: PlanarGraph => {
        other.outerFace == outerFace && other.vertexFlags == vertexFlags && other.labels == labels && other.loops == loops
      }
    }
  }

  def isAlternating_? = {
    if (vertexFlags.tail.map(_.size).forall(_ == 4) && labels.forall(_._2 == 2)) {
      (for (
        edge <- edgeSet;
        (v1, v2) = edgeVertexIncidences(edge);
        if v1 != 0 && v2 != 0
      ) yield {
        (vertexFlags(v1).indexWhere(_._1 == edge) + vertexFlags(v2).indexWhere(_._1 == edge) + 1) % 2
      }).forall(_ == 0)
    } else {
      true
    }
  }

  private def listify = PlanarGraph(outerFace, vertexFlags.toVector.map(_.toList), labels.toList, loops)

  def verify = {
    // There are many things we might check here!
    require(loops >= 0)

    require(labels.size == numberOfVertices - 1)

    require(vertexFlags(0).headOption match {
      case None         => true
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
  def numberOfInternalVertices = numberOfVertices - 1

  def vertices = 0 until numberOfVertices
  def degree(i: Int) = vertexFlags(i).size

  lazy val edgeSet = vertexFlags.flatMap(_.map(_._1)).sorted.distinct
  lazy val faceSet = (outerFace +: vertexFlags.flatMap(_.map(_._2))).sorted.distinct
  def internalFaceSet = faceSet.filterNot(vertexFlags.head.map(_._2).contains).filterNot(_ == outerFace)

  lazy val internalFaceSizes = {
    import net.tqft.toolkit.collections.Tally._
    internalFaceSet.map(faceEdgeIncidences).map(_.size).tally
  }

  def internalEdges = edgeSet.filterNot(boundaryEdges.contains)

  def numberOfEdges = edgeSet.size
  def numberOfFaces = faceSet.size
  def numberOfInternalFaces = numberOfFaces - boundaryFaces.distinct.size

  def dangle = {
    import net.tqft.toolkit.collections.Tally._
    (0 +: neighboursOf(0).filterNot(_ == 0).tally.values.toSeq).max
  }

  val dangliness: Stream[IndexedSeq[Int]] = {
    (IndexedSeq(1) ++ IndexedSeq.fill(numberOfVertices - 1)(0)) #:: dangliness.map({ d =>
      IndexedSeq.tabulate(numberOfVertices)({ i =>
        if (i == 0) {
          0
        } else {
          neighboursOf(i).map(d).sum
        }
      })
    })
  }

  def edgesAdjacentTo(vertex: Int): Seq[Int] = vertexFlags(vertex).map(_._1)
  def neighboursOf(vertex: Int) = edgesAdjacentTo(vertex).map(e => target(vertex, e))

  lazy val edgeVertexIncidences: Map[Int, (Int, Int)] = {
    val map = scala.collection.mutable.Map[Int, ListBuffer[Int]]()
    for (v <- vertices; e <- edgesAdjacentTo(v)) {
      map.getOrElseUpdate(e, ListBuffer[Int]()) += v
    }
    Map() ++ map.mapValues({ case ListBuffer(v1, v2) => (v1, v2); case _ => require(false, "invalid map: " + this + " " +  map); ??? })
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
  lazy val boundaryFaces = vertexFlags(0) match {
    case Nil   => Seq(outerFace)
    case other => other.map(_._2)
  }

  def internalFacesSizeAtMost(n: Int): Seq[Int] = faceEdgeIncidences.filter(_._2.size <= n).keys.toSeq intersect internalFaceSet
  // Note faceEdgeIncidences is a Set, so the above function doesn't count edge multiplicity.
  // But this is fine since the only kind of edge with multiplicity > 1 is a bridge (of multiplicity 2).
  lazy val hasTinyFace = internalFacesSizeAtMost(3).nonEmpty

  type EdgeFace = (Int, Int)

  def faceNeighbours(face: Int): Seq[Seq[EdgeFace]] = {
    faceBoundary(face).map(_.map(p => edgeFaceIncidences(p._2) match {
      case (`face`, f) => (p._2, f)
      case (f, `face`) => (p._2, f)
      case _           => ??? // unreachable
    }))
  }

  def edgesBetweenFaces(face1: Int, face2: Int) = {
    edgeFaceIncidences.collect({ case (e, (f1, f2)) if f1 == face1 && f2 == face2 || f1 == face2 && f2 == face1 => e })
  }
  def edgeBetweenFaces(face1: Int, face2: Int) = {
    edgesBetweenFaces(face1: Int, face2: Int).toList match {
      case List(e) => e
      case _       => require(false); ??? // this shouldn't happen
    }
  }

  type VertexEdge = (Int, Int)

  lazy val faceBoundary = {
    val map = scala.collection.mutable.HashMap[Int, Seq[Seq[VertexEdge]]]()

    { face: Int =>
      map.getOrElseUpdate(face, faceBoundary_(face))
    }
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

  def allVerticesAdjacentToFace(face: Int) = faceBoundary(face).flatten.map(_._1).distinct

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

  lazy val canonicalFormWithDefect: (PlanarGraph, Rotation) = {
    val packed = relabelEdgesAndFaces
    val labelling = Dreadnaut.canonicalLabelling(packed.nautyGraph)

    require(labelling(0) == 0)

    import net.tqft.toolkit.permutations.Permutations._
    import net.tqft.toolkit.collections.Rotate._
    import Ordering.Implicits._
    val inv = labelling.inverse

    val resultFlags = labelling.take(packed.numberOfVertices).permute(packed.vertexFlags.map(_.map(p => (inv(p._1), inv(p._2))).leastRotation))
    val newOuterFace = if (graph.numberOfBoundaryPoints == 0) {
      inv(packed.outerFace)
    } else {
      resultFlags(0)(0)._2
    }

    val vertexRotations = scala.collection.mutable.Map[VertexType, Int]().withDefaultValue(0)

    def identifyRotation[A](x: Seq[A], y: Seq[A]) = {
      if (x.isEmpty) {
        0
      } else {
        import net.tqft.toolkit.collections.Rotate._
        (0 until x.size).find(j => x.rotateLeft(j) == y).get
      }
    }

    import net.tqft.toolkit.arithmetic.Mod._

    val boundaryRotation = identifyRotation(packed.vertexFlags(0).map(p => (inv(p._1), inv(p._2))), resultFlags(0))

    // Now, we check all the vertex rotations, fixing any that were rotated by an forbidden amount... This is a hack.
    val fixedFlags = for (i <- 1 until graph.numberOfVertices) yield {
      //      val k = packed.vertexFlags(i).size
      //      val j = identifyRotation(packed.vertexFlags(i).map(p => (inv(p._1), inv(p._2))), result.vertexFlags(inv(i)))
      val k = resultFlags(i).size
      val vt = VertexType(k, graph.labels(i-1)._2, graph.labels(i-1)._1)
      val j = identifyRotation(packed.vertexFlags(labelling(i)), resultFlags(i).map(p => (labelling(p._1), labelling(p._2))))

      val j0 = j mod packed.labels(labelling(i) - 1)._1

      vertexRotations(vt) = (vertexRotations(vt) + j - j0) mod k

      resultFlags(i).rotateLeft(-j0)
    }

    //    val result = PlanarGraph(newOuterFace, resultFlags, labelling.take(packed.numberOfVertices).permute(0 +: packed.labels).tail, graph.loops)

    val fixedResult = PlanarGraph(newOuterFace, resultFlags.head +: fixedFlags, labelling.take(packed.numberOfVertices).permute((-1, -1) +: packed.labels).tail, graph.loops)

    val finalResult = DiagramSpider.graphSpider.rotate(fixedResult, -boundaryRotation).listify
    val rotation = Rotation(Map() ++ vertexRotations)

    (finalResult, rotation)

  }

  def deleting_vertex_disconnects_graph_?(vertexToDelete: Int): Boolean = {
    // Check the edges leaving the vertex, which meet the boundary, are all contiguous.
    import net.tqft.toolkit.arithmetic.Mod._

    val flag = vertexFlags(vertexToDelete)
    val valence = flag.size
    val badStuff =
      for (
        ((_, f1), i1) <- flag.zipWithIndex;
        ((_, f2), i2) <- flag.zipWithIndex;
        if i1 < i2;
        if boundaryFaces.contains(f1) && boundaryFaces.contains(f2);
        j1 <- i1 to (i2 - 1);
        if neighboursOf(vertexToDelete)(j1) != 0;
        j2 <- i2 to (i1 - 1 + valence);
        if neighboursOf(vertexToDelete)(j2 mod valence) != 0
      ) yield {
        (i1, i2, j1, j2)
      }

    //    println(vertexToDelete)
    //    println(neighboursOf(vertexToDelete))
    //    println(badStuff)

    badStuff.nonEmpty
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

  lazy val nautyGraph: ColouredGraph[Int] = {
    require(edgeSet == (numberOfVertices until numberOfVertices + edgeSet.size))
    require(faceSet == (numberOfVertices + edgeSet.size until numberOfVertices + edgeSet.size + faceSet.size))

    val edgeToFaceAdjacencies = IndexedSeq.fill(edgeSet.size)(ListBuffer[Int]())

    val vertexToEdgeAdjacencies = for (flags <- vertexFlags) yield {
      flags.map(flag => {
        edgeToFaceAdjacencies(flag._1 - numberOfVertices) += flag._2
        flag._1
      })
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

    val breakRotationalSymmetry = if (numberOfBoundaryPoints == 0) {
      IndexedSeq.empty
    } else {
      IndexedSeq(0, vertexFlags(0)(0)._1, vertexFlags(0)(0)._2)
    }

    ColouredGraph(
      numberOfVertices + edgeSet.size + faceSet.size + 3 * flagSet.size + 1,
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

  def deleteBoundaryVertex(vertexToDelete: Int) = {
    assert {
      // make sure it doesn't straddle the marked point, or have more than one contiguous run of edges touching the boundary
      // (these cases aren't needed for now, and are more complicated)

      import net.tqft.toolkit.collections.Split._
      import net.tqft.toolkit.collections.Tally._
      neighboursOf(0).rle.map(_._1).tally.apply(vertexToDelete) == 1
    }

    val newExternalFlag: Seq[(Int, Int)] = {
      val f0 = vertexFlags(0).take(neighboursOf(0).indexOf(vertexToDelete))
      val f2 = vertexFlags(0).drop(neighboursOf(0).lastIndexOf(vertexToDelete) + 1)
      val f1 = {
        // we need to take the pieces of the flag on the vertex we're about to delete that don't go to the boundary.
        // if those are non-contiguous, we need to reverse the two chunks...
        val i = neighboursOf(vertexToDelete).indexOf(0)
        import net.tqft.toolkit.collections.Rotate._
        val rotated_flag = vertexFlags(vertexToDelete).rotateLeft(i)
        val rotated_neighbours = neighboursOf(vertexToDelete).rotateLeft(i)
        rotated_flag.take(rotated_neighbours.lastIndexWhere(_ != 0) + 1).drop(rotated_neighbours.indexWhere(_ != 0))
      }
      f0 ++ f1 ++ f2
    }
    PlanarGraph(outerFace, newExternalFlag +: (vertexFlags.patch(vertexToDelete, Nil, 1).tail), labels.patch(vertexToDelete - 1, Nil, 1), loops)
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
    } else if (verticesToDelete.size == vertices.size - 1 && boundaryEdgesAndFacesToDelete.isEmpty) {
      PlanarGraph(outerFace, vertexFlags.headOption.toIndexedSeq, Seq.empty, loops - loopsToDelete)
      //      PlanarGraph.empty.copy(loops = loops - loopsToDelete)
    } else {

      val boundaryEdgesToDelete = boundaryEdgesAndFacesToDelete.map(_._1)

      require(boundaryEdgesToDelete.forall(edgeSet.contains))

      require({
        boundaryEdgesAndFacesToDelete.isEmpty ||
          boundaryEdgesAndFacesToDelete.head._2 == outerFace
      })

      if (boundaryEdgesAndFacesToDelete.isEmpty) {
        val result = PlanarGraph(
          outerFace,
          vertexFlags.zipWithIndex.collect({ case (flag, i) if !verticesToDelete.contains(i) => flag }),
          labels.zipWithIndex.collect({ case (label, i) if !verticesToDelete.contains(i + 1) => label }),
          loops - loopsToDelete)
        require(result.numberOfBoundaryPoints == boundaryEdgesAndFacesToDelete.size + graph.numberOfBoundaryPoints)
        result
      } else {
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
        val newLabels = ((-1, -1) +: labels).zipWithIndex.collect({ case (l, i) if !verticesToDelete.contains(i) => l }).tail
        val result = PlanarGraph(if (numberOfBoundaryPoints == 0) newFace else outerFace, newExternalFlag +: newInternalFlags, newLabels, loops - loopsToDelete)
        require(result.numberOfBoundaryPoints == boundaryEdgesAndFacesToDelete.size + graph.numberOfBoundaryPoints)
        result
      }
    }
  }

  val subgraphs = {
    import net.tqft.toolkit.functions.Memo
    Memo(Subgraphs.apply _)
  }

  def containsSubgraph(shape: PlanarGraph): Boolean = Subgraphs(shape).excisions.hasNext
  def containsOneOf(shapes: Seq[PlanarGraph]): Boolean = shapes.exists(s => containsSubgraph(s))

  case class Subgraphs(shape: PlanarGraph) {
    // require that every edge of shape attaches to an internal vertex
    // this is an unfortunate implementation restriction!
    require(shape.vertexFlags.head.map(_._1).distinct.size == shape.numberOfBoundaryPoints)

    private def spider = implicitly[DiagramSpider[PlanarGraph]]

    private val packedShape = shape.relabelEdgesAndFaces

    case class Excision(cut: PlanarGraph, depth: Int, rotations: Rotation) {
      //      verify

      private def verify = {
        require(cut.numberOfBoundaryPoints - 2 * depth - shape.numberOfBoundaryPoints == graph.numberOfBoundaryPoints)

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

    lazy val cachedExcisions = excisions.toStream
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
          val newMap = scala.collection.mutable.Map[VertexType, Int]().withDefault(_ => 0)
          for ((r, i) <- vertexRotations.zipWithIndex) {
            val k = packedShape.degree(i + 1)
            val vt = VertexType(k, packedShape.labels(i)._2, packedShape.labels(i)._1)
            newMap(vt) = newMap(vt) + r
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
          import net.tqft.toolkit.collections.Rotate._

          def compareSelfLoops(sourceFlag: Seq[(Int, Int)], targetFlag: Seq[(Int, Int)]): Boolean = {
            sourceFlag.forall({
              case (e, _) => sourceFlag.zipWithIndex.filter(_._1._1 == e).map(_._2).map(targetFlag(_)._1).distinct.size == 1
            })
          }

          if (partial.map(sourceVertex - 1) == targetVertex && (((partial.vertexRotations(sourceVertex - 1) - rotation) mod packedShape.degree(sourceVertex)) == 0)) {
            // already done this vertex
            //            Logging.info(" ... already done")
            Some(partial)
          } else if (targetVertex == 0 ||
            partial.map.contains(targetVertex) ||
            partial.map(sourceVertex - 1) != -1 ||
            packedShape.degree(sourceVertex) != graph.degree(targetVertex) ||
            packedShape.labels(sourceVertex - 1) != graph.labels(targetVertex - 1) ||
            !compareSelfLoops(packedShape.vertexFlags(sourceVertex), graph.vertexFlags(targetVertex).rotateLeft(rotation)) ||
            (rotation mod packedShape.labels(sourceVertex - 1)._1) != 0) {
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

              require(cut.numberOfBoundaryPoints == geodesic.size * 2 + graph.numberOfBoundaryPoints)

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
              //                  Logging.info(" ... no geodesics found")
              Iterator.empty
            }
          } else {
            // ooops, swallowed something up, ignore this one

            //                Logging.info("... there's something inside!")
            //                for (f <- packedShape.internalFaceSet.map(f => (f, packedShape.faceBoundary(f), partial.faceMap(f), graph.faceBoundary(partial.faceMap(f))))) Logging.info(f)
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
            //                Logging.info(excision)
            excision
          }
        }
      }

      if (packedShape.loops <= graph.loops) {
        if (packedShape.numberOfInternalVertices == 0) {
          Iterator(Excision(graph.copy(loops = graph.loops - packedShape.loops), 0, Rotation(Map.empty)))
        } else {
          if (packedShape.internalFaceSizes.forall({
            case (size, count) => count <= graph.internalFaceSizes.getOrElse(size, 0)
          })) {
            extendPartialExcision(
              PartialMap(
                Array.fill(packedShape.numberOfVertices - 1)(-1),
                Array.fill(packedShape.numberOfVertices - 1)(-1)))
          } else {
            Iterator.empty
          }
        }
      } else {
        Iterator.empty
      }

    }
  }
}

object PlanarGraph {
  implicit def ordering: Ordering[PlanarGraph] = {
    import scala.math.Ordering.Implicits._
    Ordering.by({ x: PlanarGraph => (x.outerFace, x.vertexFlags, x.labels, x.loops) })
  }

  def spider = implicitly[Spider[PlanarGraph]]

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

  private object PickleParser extends RegexParsers with JavaTokenParsers {
    val sequenceTypes = "Seq" | "List" | "Vector" | "ArrayBuffer"

    def list[A](parsable: Parser[A]): Parser[Seq[A]] = sequenceTypes ~> "(" ~> whitespace ~> repsep(parsable, "," ~ whitespace) <~ whitespace <~ ")"
    def seq[A](parsable: Parser[A]): Parser[Seq[A]] = list(parsable) ^^ { _.toSeq }
    def indexedSeq[A](parsable: Parser[A]): Parser[IndexedSeq[A]] = list(parsable) ^^ { _.toIndexedSeq }
    def option[A](parsable: Parser[A]): Parser[Option[A]] = ("None" ^^ { _ => None }) | ("Some(" ~> parsable <~ ")" ^^ { Some(_) })
    def optional[A](parsable: Parser[Option[A]]) : Parser[Option[A]] = parsable | "" ^^ { _ => None }
    
    def pair[A](parsable: Parser[A]): Parser[(A, A)] = "(" ~> whitespace ~> parsable ~ "," ~ whitespace ~ parsable <~ ")" ^^ {
      case a1 ~ "," ~ whitespace ~ a2 => (a1, a2)
    }

    def whitespaceCharacter: Parser[String] = " " | "\n" | "\t"
    def whitespace = whitespaceCharacter.*

    def int = wholeNumber ^^ { _.toInt }
    def quotedString: Parser[String] = "\"" ~> "[^\",\r\n]".r <~ "\"" ^^ { s => s }

    def planarGraph: Parser[PlanarGraph] = ("PlanarGraph(" ~> whitespace ~>
      (int <~ "," <~ whitespace) ~
      (indexedSeq(seq(pair(int))) <~ "," <~ whitespace) ~
      (seq(pair(int)) <~ "," <~ whitespace) ~
      (int) ~
      optional("," ~> whitespace ~> option(quotedString)) <~ whitespace <~ ")") ^^ {
        case outerFace ~ vertexFlags ~ labels ~ loops ~ comment => PlanarGraph(outerFace, vertexFlags, labels, loops, comment)
      }

  }

  def graphSeqFromString(string: String): Seq[PlanarGraph] = {
    import PickleParser._

    val parse = parseAll(seq(planarGraph), string)
    require(parse.successful, "Parsing failed:\n" + string)
    parse.get
  }

  def fromString(s: String) = {
    import PickleParser._

    val parse = parseAll(planarGraph, s)
    require(parse.successful, "Parsing failed:\n" + s)
    parse.get
  }

  private def polygon_(k: Int) = {
    if (k == 0) {
      loop
    } else {
      import net.tqft.toolkit.arithmetic.Mod._
      val flags = IndexedSeq.tabulate(k)(i => (i + k + 1, i + 3 * k + 1)) +:
        IndexedSeq.tabulate(k)(i => IndexedSeq((i + 2 * k + 1, 4 * k + 1), (i + k + 1, (i + 1 mod k) + 3 * k + 1), ((i - 1 mod k) + 2 * k + 1, i + 3 * k + 1)))
      PlanarGraph(3 * k + 1, flags, IndexedSeq.fill(k)((1, 0)), 0)
    }
  }

  val polygon = {
    import net.tqft.toolkit.functions.Memo._
    (polygon_ _).memo
  }

  private def star_(t: (Int, Int, Int)) = {
    val (k, l, r) = t
    val flags = IndexedSeq(
      Seq.tabulate(k)(i => (i + 2, i + k + 2)),
      Seq.tabulate(k)(i => (i + 2, ((i + 1) % k) + k + 2)).reverse)
    PlanarGraph(k + 2, flags, IndexedSeq((r, l)), 0)
  }

  private val starCache = {
    import net.tqft.toolkit.functions.Memo
    Memo(star_ _)
  }

  def star(v: VertexType): PlanarGraph = star(v.perimeter, v.label, v.allowedRotationStep)
  def star(valence: Int, label: Int = 0, allowedRotation: Int = 1) = starCache((valence, label, allowedRotation))

  val trivalentVertex = star(3)

  val I = spider.multiply(spider.rotate(star(3), 1), spider.rotate(star(3), -1), 1)
  val H = spider.rotate(I, 1)

  val theta = spider.multiply(star(3), star(3), 3)
  val tetrahedron = spider.multiply(star(3), polygon(3), 3)
  val cube = spider.multiply(polygon(4), polygon(4), 4)

  val twoSquares = spider.multiply(polygon(4), H, 2)
  val pentaSquare = spider.multiply(PlanarGraph.polygon(4), spider.rotate(spider.multiply(PlanarGraph.trivalentVertex, PlanarGraph.H, 1), -1), 2)
  val pentapent = spider.multiply(PlanarGraph.polygon(5), spider.rotate(spider.multiply(PlanarGraph.trivalentVertex, PlanarGraph.H, 1), -1), 2)

  val pentafork = spider.multiply(PlanarGraph.polygon(5), star(3), 1)
  val hexafork = spider.multiply(PlanarGraph.polygon(6), star(3), 1)

  val tetravalentVertex = star(4)
  val bowtie = spider.stitch(spider.stitch(tetravalentVertex))

  val crossing = star(4, 0, 2)
  val inverseCrossing = spider.rotate(crossing, 1)

  val positiveTwistedLoop = spider.stitch(spider.stitch(crossing))
  val positiveTwistedTheta = spider.multiply(I, crossing, 4)
  val twistedTetrahedron = spider.multiply(polygon(4), crossing, 4)

  val hopfStrand = spider.multiply(crossing, crossing, 3)
  val hopfLink = spider.stitch(hopfStrand)

  val positiveTwist = spider.stitch(crossing)
  val negativeTwist = spider.stitch(spider.rotate(crossing, 1))

  val positiveTwistedTrivalentVertex = spider.multiply(spider.rotate(crossing, 1), trivalentVertex, 2)
  val negativeTwistedTrivalentVertex = spider.multiply(crossing, trivalentVertex, 2)

  val twistedH = spider.multiply(H, crossing, 2)

  val Reidemeister1a = Seq(strand, positiveTwist)
  val Reidemeister1b = Seq(strand, negativeTwist)
  val Reidemeister2 = Seq(two_strands_vertical, spider.multiply(crossing, spider.rotate(crossing, 1), 2))

  lazy val Reidemeister3 = {
    val tangle = spider.multiply(spider.rotate(spider.multiply(crossing, spider.rotate(crossing, 1), 1), -1), crossing, 2)
    Seq(tangle, spider.rotate(tangle, 3))
  }
  lazy val Reidemeister4a = Seq(
    spider.rotate(spider.multiply(trivalentVertex, crossing, 1), 1),
    spider.multiply(spider.rotate(spider.multiply(crossing, spider.rotate(crossing, 1), 1), -1), trivalentVertex, 2))
  lazy val Reidemeister4b = Seq(
    spider.rotate(spider.multiply(trivalentVertex, spider.rotate(crossing, 1), 1), 1),
    spider.multiply(spider.rotate(spider.multiply(spider.rotate(crossing, 1), crossing, 1), -1), trivalentVertex, 2))

  lazy val dodecahedron = {
    val penta5fork = {
      def f(g: PlanarGraph) = spider.rotate(spider.multiply(star(3), g, 1), 1)
      f(f(f(f(f(polygon(5))))))
    }
    spider.multiply(penta5fork, spider.rotate(penta5fork, 1), 10)
  }

}
