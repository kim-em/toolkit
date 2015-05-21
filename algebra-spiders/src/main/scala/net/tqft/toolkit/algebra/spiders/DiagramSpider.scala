package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.graphs.Dreadnaut

trait CanonicalLabellingWithDefect[A, D] {
  def canonicalFormWithDefect(a: A): (A, D)
}

case class Rotation(vertexRotations: Map[Int, Int])

trait DiagramSpider[A] extends Spider[A] with CanonicalLabellingWithDefect[A, Rotation]

object DiagramSpider {
  implicit val graphSpider: DiagramSpider[PlanarGraph] = {
    new DiagramSpider[PlanarGraph] {
      override def empty = PlanarGraph.empty
      override def strand = PlanarGraph.strand
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
        PlanarGraph(newOuterFace, graph.vertexFlags.updated(0, graph.vertexFlags(0).rotateLeft(k)), graph.labels, graph.loops)
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

  def assembleAlongPlanarPartition(partition: Seq[Seq[Int]], ds: Seq[PlanarGraph]): PlanarGraph = {
    // partition is a planar partition of 0, ..., n-1
    // Validity checks
    require(partition.head.head == 0, "Begin partition from 0")
    require(isContiguous(partition.flatten.sorted), "Not a planar partition")
    require(isSorted(for (s <- partition) yield s.head), "Give partition in anticlockwise sequential order")
    require(partition.length == ds.length, "Partition does not match graphs")
    for (k <- 0 until partition.length) {
      require(isSorted(partition(k)), s"Partition cell $k needs to be sorted")
      require(partition(k).length == ds(k).numberOfBoundaryPoints, s"Graph in index $k does not have the correct number of boundary points")
    }

    def isSorted(S: Seq[Int]): Boolean = (S == S.sorted)
    def isContiguous(S: Seq[Int]): Boolean = (S == (S.head to S.last).toSeq)
    def mod(a: Int, b: Int): Int = ((a % b) + b) % b

    def assemble(partition: Seq[Seq[Int]], ds: Seq[PlanarGraph]): PlanarGraph = {
      if (partition.length == 1) {
        require(isContiguous(partition.head), "Invalid partition")
        return ds.head
      } else if (isContiguous(partition.head)) {
        val newPartition = partition.tail.map(_.map((x: Int) => x - partition.head.length))
        return DiagramSpider.graphSpider.tensor(assemble(newPartition, ds.tail), ds.head)
      } else {
        val n = partition.flatten.length
        val zipped = partition zip ds
        val newPartitionAndGraphs = zipped.map((X: Tuple2[Seq[Int], PlanarGraph]) =>
          (X._1.map((x: Int) => mod(x - 1, n)).sorted, if (partition.head == X._1) DiagramSpider.graphSpider.rotate(X._2, 1) else X._2)).
          sortWith((X1: Tuple2[Seq[Int], PlanarGraph], X2: Tuple2[Seq[Int], PlanarGraph]) => X1._1.head < X2._1.head).
          unzip
        return DiagramSpider.graphSpider.rotate(assemble(newPartitionAndGraphs._1, newPartitionAndGraphs._2), -1)
      }
    }

    assemble(partition, ds)
  }

  implicit class DiskDiagramSpider[A](spider: DiagramSpider[A]) extends Spider.DiskSpider[A](spider) with DiagramSpider[Disk[A]] {
    override def canonicalFormWithDefect(disk: Disk[A]) = {
      val (result, defect) = spider.canonicalFormWithDefect(disk.contents)
      (Disk(disk.circumference, result), defect)
    }

  }

}