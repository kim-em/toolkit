package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.graphs.ColouredGraph
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import scala.collection.mutable.ListBuffer

// flags veer to the left
// edges are ordered clockwise around each vertex
case class PlanarGraph(vertexFlags: IndexedSeq[Seq[(Int, Int)]], loops: Int) { graph =>

  def numberOfBoundaryPoints = vertexFlags(0).size
  def outerFace = vertexFlags(0).headOption.map(_._2).getOrElse(0)

  val numberOfVertices = vertexFlags.size

  def vertices = 0 until numberOfVertices
  def degree(i: Int) = vertexFlags(i).size

  lazy val edgeSet = vertexFlags.flatMap(_.map(_._1)).sorted.distinct
  lazy val faceSet = vertexFlags.flatMap(_.map(_._2)).sorted.distinct
  def numberOfEdges = edgeSet.size
  def numberOfFaces = faceSet.size

  def edgesAdjacentTo(vertex: Int): Seq[Int] = vertexFlags(vertex).map(_._1)
  def neighboursOf(vertex: Int) = edgesAdjacentTo(vertex).map(e => target(vertex, e))

  lazy val edgeIncidences: Map[Int, List[Int]] = {
    val map = scala.collection.mutable.Map[Int, ListBuffer[Int]]()
    for (v <- vertices; e <- edgesAdjacentTo(v)) {
      map.getOrElseUpdate(e, ListBuffer[Int]()) += v
    }
    Map() ++ map.mapValues(_.toList)
  }
  lazy val maxEdgeLabel = edgeIncidences.keysIterator.max
  lazy val maxFaceLabel = vertexFlags.iterator.flatMap(_.map(_._2)).max

  def target(source: Int, edge: Int): Int = {
    val ends = edgeIncidences(edge)
    if (ends.head == source) {
      ends(1)
    } else {
      ends.head
    }
  }

  def boundaryFaces = vertexFlags(0).map(_._2)

  def faceNeighbours(face: Int) = {
    for (i <- vertices; (e, `face`) <- vertexFlags(i)) yield vertexFlags(target(i, e)).find(_._1 == e).get._2
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
          val geodesic = geodesicsToOuterFace(???).next
          
          val excision = ???
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
    import net.tqft.toolkit.arithmetic.Mod._

    val flags = IndexedSeq(
      Seq.tabulate(k)(i => (i + 2, i + k + 2)),
      Seq.tabulate(k)(i => (i + 2, (i + 1 mod k) + k + 2)).reverse)
    PlanarGraph(flags, 0)
  }

  val I = spider.multiply(spider.rotate(star(3), 1), spider.rotate(star(3), -1), 1)
  val H = spider.rotate(I, 1)

}