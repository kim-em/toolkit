package net.tqft.toolkit.algebra.graphs

import scala.collection.mutable.ListBuffer
import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.modules.Module
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.RationalFunction

sealed trait Label[L]
case class BoundaryPoint[L](index: Int) extends Label[L]
case class InternalVertex[L](label: L) extends Label[L]

object Label {
  implicit def labelOrdering[L: Ordering] = new Ordering[Label[L]] {
    override def compare(a: Label[L], b: Label[L]) = (a, b) match {
      case (BoundaryPoint(ai), BoundaryPoint(bi)) => ai - bi
      case (BoundaryPoint(_), InternalVertex(_)) => -1
      case (InternalVertex(_), BoundaryPoint(_)) => 1
      case (InternalVertex(al), InternalVertex(bl)) => implicitly[Ordering[L]].compare(al, bl)
    }
  }
}
case class PlanarGraph[L: Ordering](
  numberOfBoundaryPoints: Int,
  vertexLabels: IndexedSeq[Label[L]],
  vertexFlags: IndexedSeq[IndexedSeq[(Int, Int)]]) {

  val numberOfVertices = vertexLabels.size
  def vertices = 0 until numberOfVertices

  def edgesAdjacentTo(vertex: Int): IndexedSeq[Int] = vertexFlags(vertex).map(_._1)

  lazy val edges = {
    val map = scala.collection.mutable.Map[Int, ListBuffer[Int]]()
    for (v <- vertices; e <- edgesAdjacentTo(v)) {
      map.getOrElseUpdate(e, ListBuffer[Int]()) += v
    }
    Map() ++ map.mapValues(_.toList)
  }
  lazy val maxEdgeLabel = edges.keysIterator.max

  lazy val maxFaceLabel = vertexFlags.iterator.flatMap(_.map(_._2)).max

  def target(source: Int, edge: Int): Int = edges(edge).find(_ != source).get
  def faceOnRight(source: Int, edge: Int): Int = vertexFlags(target(source, edge)).find(_._1 == edge).get._2
  
  lazy val boundaryVertices = {
    val a = Array[Int](numberOfBoundaryPoints)
    for ((BoundaryPoint(i), j) <- vertexLabels.zipWithIndex) {
      a(i) = j
    }
    a.toSeq    
  }
  lazy val boundaryEdges = {
    val a = Array[Int](numberOfBoundaryPoints)
    for ((BoundaryPoint(i), j) <- vertexLabels.zipWithIndex) {
      a(i) = vertexFlags(j).find(_._2 == 0).get._1
    }
    a.toSeq
  }
  lazy val boundaryFaces = {
    val a = Array[Int](numberOfBoundaryPoints)
    for ((BoundaryPoint(i), j) <- vertexLabels.zipWithIndex) {
      a(i) = vertexFlags(j).find(p => faceOnRight(j, p._1) == 0).get._2
    }
    a.toSeq
  }

  lazy val nautyGraph: ColouredGraph[Label[L]] = {
    val adjacencies = for (v <- vertices) yield {
      for (e <- edgesAdjacentTo(v)) yield {
        target(v, e)
      }
    }
    ColouredGraph(vertexLabels.size, adjacencies, vertexLabels)
  }
}

object PlanarGraph {
  def build[L: Ordering] = Builder[L]

  case class Builder[L: Ordering] {
    def spider = implicitly[Spider[PlanarGraph[L]]]

    def star(k: Int, label: L) = {
      import net.tqft.toolkit.arithmetic.Mod._

      val labels: IndexedSeq[Label[L]] = IndexedSeq.tabulate(k)(i => BoundaryPoint[L](i)) :+ InternalVertex(label)
      val flags = (for (i <- 0 until k) yield {
        IndexedSeq((i, 0), (i - 1 mod k, i - 1 mod k), (k, i))
      }) :+ IndexedSeq.tabulate(k)(i => (i, i - 1 mod k))
      PlanarGraph(k, labels, flags)
    }

    def I(labelTop: L, labelBottom: L) = spider.multiply(star(3, labelTop), star(3, labelBottom), 1)
  }
}

trait Spider[A] {
  def rotate(a: A, k: Int): A
  def tensor(a1: A, a2: A): A
  def stitch(a: A): A
  def canonicalForm(a: A): A
  def circumference(a: A): Int

  def stitchAt(a: A, k: Int): A = rotate(stitch(rotate(a, 1 - k)), k - 1)
  def stitchesAt(a: A, k: Int, m: Int): A = (k until k - m by -1).foldLeft(a)(stitchAt)

  def multiply(a1: A, a2: A, m: Int): A = {
    stitchesAt(tensor(a1, a2), circumference(a1), m)
  }

  def innerProduct(a1: A, a2: A): A = {
    multiply(a1, a2, circumference(a1))
  }
}

case class Disk[C](circumference: Int, contents: C)

object Spider {

  implicit def graphSpider[L: Ordering]: Spider[PlanarGraph[L]] = {
    new Spider[PlanarGraph[L]] {
      override def circumference(graph: PlanarGraph[L]) = graph.numberOfBoundaryPoints
      override def rotate(graph: PlanarGraph[L], k: Int) = {
        import net.tqft.toolkit.arithmetic.Mod._

        graph.copy(vertexLabels = graph.vertexLabels.map({
          case BoundaryPoint(i) => BoundaryPoint[L](i + k mod graph.numberOfBoundaryPoints)
          case label: InternalVertex[L] => label
        }))
      }
      override def tensor(graph1: PlanarGraph[L], graph2: PlanarGraph[L]) = {
        def labels = graph1.vertexLabels ++ graph2.vertexLabels.map({
          case BoundaryPoint(i) => BoundaryPoint[L](i + graph1.numberOfBoundaryPoints)
          case label: InternalVertex[L] => label
        })
        def newFlags1 = {
          graph1.vertexFlags.map({ edges1 =>
            val B = graph1.boundaryEdges.last
            edges1.map({
              case (B, 0) => (graph2.boundaryEdges.head, 0)
              case x => x
            })
          })
        }
        def newFlags2 = {
          graph2.vertexFlags.map({ edges1 =>
            val B = graph2.boundaryEdges.last
            val F = graph2.boundaryFaces.last
            edges1.map({
              case (B, 0) => (graph1.boundaryEdges.head, 0)
              case (e, 0) => (graph1.maxEdgeLabel + 1 + e, 0)
              case (e, F) => (graph1.maxEdgeLabel + 1 + e, graph1.boundaryFaces.last)
              case (e, f) => (graph1.maxEdgeLabel + 1 + e, graph1.maxFaceLabel + 1 + f)
            })
          })
        }
        def flags = newFlags1 ++ newFlags2
        PlanarGraph[L](graph1.numberOfBoundaryPoints + graph2.numberOfBoundaryPoints, labels, flags)
      }
      override def stitch(graph: PlanarGraph[L]) = {
        def labels = ???
        def flags = ???
        PlanarGraph[L](graph.numberOfBoundaryPoints - 2, labels, flags)
      }
      override def canonicalForm(graph: PlanarGraph[L]) = {
        ???
      }
    }
  }

  implicit class DiskSpider[A](spider: Spider[A]) extends Spider[Disk[A]] {
    override def rotate(disk: Disk[A], k: Int) = Disk(disk.circumference, spider.rotate(disk.contents, k))
    override def tensor(disk1: Disk[A], disk2: Disk[A]) = Disk(disk1.circumference + disk2.circumference, spider.tensor(disk1.contents, disk2.contents))
    override def stitch(disk: Disk[A]) = Disk(disk.circumference - 2, spider.stitch(disk.contents))
    override def canonicalForm(disk: Disk[A]) = Disk(disk.circumference, spider.canonicalForm(disk.contents))
    override def circumference(disk: Disk[A]) = disk.circumference
  }

}

trait LinearSpider[R, M] extends Spider[M] with Module[R, M]

object LinearSpider {
  def rationalSpider = implicitly[LinearSpider[RationalFunction[Int], Disk[Map[PlanarGraph[Int], RationalFunction[Int]]]]]

  implicit def lift[A: Spider, R: Ring]: LinearSpider[R, Map[A, R]] = {
    val spider = implicitly[Spider[A]]
    val ring = implicitly[Ring[R]]
    new LinearSpider[R, Map[A, R]] {

      private def mapKeys(f: A => A)(map: TraversableOnce[(A, R)]) = {
        val newMap = scala.collection.mutable.Map[A, R]()
        for ((a, r) <- map) {
          val s = f(a)
          newMap(s) = newMap.get(s).map(v => ring.add(v, r)).getOrElse(r)
        }
        Map() ++ newMap.filter(_._2 != ring.zero)
      }

      override def rotate(map: Map[A, R], k: Int) = map.map(p => (spider.rotate(p._1, k), p._2))
      override def tensor(map1: Map[A, R], map2: Map[A, R]) = {
        val newMap = scala.collection.mutable.Map[A, R]()
        for ((a, r) <- map1; (b, s) <- map2) {
          val t = spider.tensor(a, b)
          val p = ring.multiply(r, s)
          newMap(t) = newMap.get(t).map(v => ring.add(v, p)).getOrElse(p)
        }
        Map() ++ newMap.filter(_._2 != ring.zero)
      }
      override def stitch(map: Map[A, R]) = mapKeys(spider.stitch)(map)
      override def canonicalForm(map: Map[A, R]) = mapKeys(spider.canonicalForm)(map)
      override def circumference(map: Map[A, R]) = spider.circumference(map.head._1)

      override def zero = Map.empty
      override def add(map1: Map[A, R], map2: Map[A, R]) = mapKeys(x => x)(map1.iterator ++ map2.iterator)
      override def scalarMultiply(r: R, map: Map[A, R]) = map.mapValues(v => ring.multiply(r, v)).filter(_._2 != ring.zero)
      override def negate(map: Map[A, R]) = map.mapValues(v => ring.negate(v))
    }
  }

  implicit def diskLinearSpider[A, R, M](implicit spider: LinearSpider[R, M]): LinearSpider[R, Disk[M]] = new Spider.DiskSpider(spider) with LinearSpider[R, Disk[M]] {
    override def zero = ???
    override def add(disk1: Disk[M], disk2: Disk[M]) = Disk(disk1.circumference, spider.add(disk1.contents, disk2.contents))
    override def scalarMultiply(r: R, disk: Disk[M]) = Disk(disk.circumference, spider.scalarMultiply(r, disk.contents))
    override def negate(disk: Disk[M]) = Disk(disk.circumference, spider.negate(disk.contents))
  }

}

