package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.graphs.Dreadnaut

trait CanonicalLabellingWithDefect[A, D] {
  def canonicalFormWithDefect(a: A): (A, D)
}

case class Rotation(vertexRotations: Map[VertexType, Int])

trait DiagramSpider[A] extends Spider[A] with CanonicalLabellingWithDefect[A, Rotation]

object DiagramSpider {
  implicit val graphSpider: DiagramSpider[PlanarGraph] = {
    new DiagramSpider[PlanarGraph] {
      override def empty = PlanarGraph.empty
      override def strand = PlanarGraph.strand
      override def circumference(graph: PlanarGraph) = graph.numberOfBoundaryPoints
      override def rotate(graph: PlanarGraph, k: Int) = {
       import net.tqft.toolkit.arithmetic.Mod._
       val k0 = k mod graph.numberOfBoundaryPoints
        import net.tqft.toolkit.collections.Rotate._
        val newOuterFace = {
          if (k0 == 0 || graph.numberOfBoundaryPoints == 0) {
            graph.outerFace
          } else {            
            graph.vertexFlags(0)(k0)._2
          }
        }
        PlanarGraph(newOuterFace, graph.vertexFlags.updated(0, graph.vertexFlags(0).rotateLeft(k0)), graph.labels, graph.loops)
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
          PlanarGraph(graph1.outerFace, flags, graph1.labels ++ graph2.labels, graph1.loops + graph2.loops)
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

          PlanarGraph(
              relabelFace(graph.outerFace), 
              graph.vertexFlags.head.dropRight(2) +: graph.vertexFlags.tail.map(_.map(p => (p._1, relabelFace(p._2)))), 
              graph.labels,
              graph.loops + 1)
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

          PlanarGraph(graph.outerFace, flags, graph.labels, graph.loops)
        }
      }

      override def canonicalFormWithDefect(graph: PlanarGraph) = {
    	  graph.canonicalFormWithDefect
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