package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.graphs.Dreadnaut

trait CanonicalLabellingWithDefect[A, D] {
  def canonicalFormWithDefect(a: A): (A, D)
}

case class Rotation(vertexRotations: Map[Int, Int])

trait DiagramSpider[A] extends Spider[A] with CanonicalLabellingWithDefect[A, Rotation]

object DiagramSpider {
  implicit def graphSpider: DiagramSpider[PlanarGraph] = {
    new DiagramSpider[PlanarGraph] {
      override def empty = PlanarGraph.empty
      override def circumference(graph: PlanarGraph) = graph.numberOfBoundaryPoints
      override def rotate(graph: PlanarGraph, k: Int) = {
        import net.tqft.toolkit.collections.Rotate._
        val newOuterFace = {
          if (k == 0 || graph.numberOfBoundaryPoints == 0) {
            graph.outerFace
          } else {
            import net.tqft.toolkit.arithmetic.Mod._
            graph.vertexFlags(0)(k mod graph.numberOfBoundaryPoints)._2
          }
        }
        PlanarGraph(newOuterFace, graph.vertexFlags.updated(0, graph.vertexFlags(0).rotateLeft(k)), graph.loops)
      }
      override def tensor(graph1: PlanarGraph, graph2: PlanarGraph) = {
        if (graph1.numberOfEdges == 0) {
          if (graph1.loops == 0) {
            graph2
          } else {
            graph2.copy(loops = graph1.loops + graph2.loops)
          }
        } else {
          def flags = {
            val ne = graph1.maxEdgeLabel
            val nf = graph1.maxFaceLabel
            def relabelFlag: ((Int, Int)) => (Int, Int) = {
              case (e, f) if f == graph2.outerFace => (e + 1 + ne, graph1.outerFace)
              case (e, f) => (e + 1 + ne, f + 1 + nf)
            }
            val externalFlag = {
              graph2.vertexFlags.head.map(relabelFlag) ++ graph1.vertexFlags.head
            }
            (externalFlag +: graph1.vertexFlags.tail) ++ graph2.vertexFlags.tail.map(_.map(relabelFlag))
          }
          PlanarGraph(graph1.outerFace, flags, graph1.loops + graph2.loops)
        }
      }
      override def stitch(graph: PlanarGraph) = {
        require(graph.numberOfBoundaryPoints >= 2)

        val e0 = graph.vertexFlags(0).last._1
        val e1 = graph.vertexFlags(0).secondLast._1

        if (e0 == e1) {
          val f1 = graph.vertexFlags(0).last._2
          def relabelFace(f: Int) = {
            if (f == f1) {
              graph.outerFace
            } else {
              f
            }
          }

          PlanarGraph(relabelFace(graph.outerFace), graph.vertexFlags.head.dropRight(2) +: graph.vertexFlags.tail.map(_.map(p => (p._1, relabelFace(p._2)))), graph.loops + 1)
        } else {
          val f1 = graph.vertexFlags(0).secondLast._2
          def relabelFace(f: Int) = {
            if (f == f1) {
              graph.outerFace
            } else {
              f
            }
          }
          val emin = if (e0 < e1) e0 else e1

          def flags = (graph.vertexFlags.head.dropRight(2) +: graph.vertexFlags.tail).map(_.map({
            case (e, f) if e == e0 || e == e1 => (emin, relabelFace(f))
            case (e, f) => (e, relabelFace(f))
          }))

          PlanarGraph(graph.outerFace, flags, graph.loops)
        }
      }

      override def canonicalFormWithDefect(graph: PlanarGraph) = {
        val packed = graph.relabelEdgesAndFaces
        val nautyGraph = packed.nautyGraph
        val labelling = Dreadnaut.canonicalLabelling(nautyGraph)

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
        val result = PlanarGraph(newOuterFace, resultFlags, graph.loops)

        val vertexRotations = scala.collection.mutable.Map[Int, Int]().withDefaultValue(0)

        def identifyRotation[A](x: Seq[A], y: Seq[A]) = {
          if (x.isEmpty) {
            0
          } else {
            import net.tqft.toolkit.collections.Rotate._
            (0 until x.size).find(j => x.rotateLeft(j) == y).get
          }
        }

        import net.tqft.toolkit.arithmetic.Mod._

        val boundaryRotation = identifyRotation(packed.vertexFlags(0).map(p => (inv(p._1), inv(p._2))), result.vertexFlags(0))

        for (i <- 1 until graph.numberOfVertices) {
          val k = packed.vertexFlags(i).size
          val j = identifyRotation(packed.vertexFlags(i).map(p => (inv(p._1), inv(p._2))), result.vertexFlags(inv(i)))
          vertexRotations(k) = vertexRotations(k) + j mod k
        }

        (rotate(result, -boundaryRotation), Rotation(Map() ++ vertexRotations))
      }
    }
  }

  implicit class DiskDiagramSpider[A](spider: DiagramSpider[A]) extends Spider.DiskSpider[A](spider) with DiagramSpider[Disk[A]] {
    override def canonicalFormWithDefect(disk: Disk[A]) = {
      val (result, defect) = spider.canonicalFormWithDefect(disk.contents)
      (Disk(disk.circumference, result), defect)
    }

  }

}