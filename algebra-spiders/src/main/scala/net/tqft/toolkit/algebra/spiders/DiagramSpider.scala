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
      override def circumference(graph: PlanarGraph) = graph.numberOfBoundaryPoints
      override def rotate(graph: PlanarGraph, k: Int) = {
        import net.tqft.toolkit.collections.Rotate._
        PlanarGraph(graph.vertexFlags.updated(0, graph.vertexFlags(0).rotateLeft(k)), graph.loops)
      }
      override def tensor(graph1: PlanarGraph, graph2: PlanarGraph) = {
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
        PlanarGraph(flags, graph1.loops + graph2.loops)
      }
      override def stitch(graph: PlanarGraph) = {
        require(graph.numberOfBoundaryPoints >= 2)

        val f1 = graph.vertexFlags(0).secondLast._2
        def relabelFace(f: Int) = {
          if (f == f1) {
            graph.outerFace
          } else {
            f
          }
        }
        val e0 = graph.vertexFlags(0).last._1
        val e1 = graph.vertexFlags(0).secondLast._1

        if (e0 == e1) {
          PlanarGraph(graph.vertexFlags.head.dropRight(2) +: graph.vertexFlags.tail, graph.loops + 1)
        } else {
          val emin = if (e0 < e1) e0 else e1

          def flags = (graph.vertexFlags.head.dropRight(2) +: graph.vertexFlags.tail).map(_.map({
            case (e, f) if e == e0 || e == e1 => (emin, f)
            case (e, f) => (e, relabelFace(f))
          }))

          PlanarGraph(flags, graph.loops)
        }
      }

      override def canonicalFormWithDefect(graph: PlanarGraph) = {
        val packed = graph.relabelEdgesAndFaces
        println(packed)
        val nautyGraph = packed.nautyGraph
        println(nautyGraph)
        val labelling = Dreadnaut.canonicalLabelling(nautyGraph)
        
        require(labelling(0) == 0)
        
        println(labelling)
        import net.tqft.toolkit.permutations.Permutations._
        import net.tqft.toolkit.collections.Rotate._
        import net.tqft.toolkit.collections.LexicographicOrdering._
        val inv = labelling.inverse
        val result = PlanarGraph(labelling.take(packed.numberOfVertices).permute(packed.vertexFlags.map(_.map(p => (inv(p._1), inv(p._2))).leastRotation)), graph.loops)

        val vertexRotations = scala.collection.mutable.Map[Int, Int]().withDefaultValue(0)

        def identifyRotation[A](x: Seq[A], y: Seq[A]) = {
          println("identifying rotation for: " + x + " and " + y)
          if (x.isEmpty) {
            0
          } else {
            import net.tqft.toolkit.collections.Rotate._
            (0 until x.size).find(j => x.rotateLeft(j) == y).get
          }
        }

        import net.tqft.toolkit.arithmetic.Mod._

        val boundaryRotation = identifyRotation(packed.vertexFlags(0).map(p => (inv(p._1), inv(p._2))), result.vertexFlags(0))
//        require(boundaryRotation == 0)

        for (i <- 1 until graph.numberOfVertices) {
          val k = packed.vertexFlags(i).size
          val j = identifyRotation(packed.vertexFlags(i).map(p => (inv(p._1), inv(p._2))), result.vertexFlags(inv(i)))
          vertexRotations(k) = vertexRotations(k) + j mod k
        }

        (result, Rotation(Map() ++ vertexRotations))
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