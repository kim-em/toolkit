package net.tqft.toolkit.algebra.graphs

import scala.collection.mutable.ListBuffer
import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.modules.Module
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.RationalFunction
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.Field

case class RotationDefects[A](contents: A, totalRotations: Map[Int, Int])

case class PlanarGraph(
  numberOfBoundaryPoints: Int,
  vertexLabels: IndexedSeq[Int],
  vertexFlags: IndexedSeq[Seq[(Int, Int)]]) {

  val numberOfVertices = vertexFlags.size
  def vertices = 0 until numberOfVertices

  def edgesAdjacentTo(vertex: Int): Seq[Int] = vertexFlags(vertex).map(_._1)

  lazy val edgeIncidences: Map[Int, List[Int]] = {
    val map = scala.collection.mutable.Map[Int, ListBuffer[Int]]()
    for (v <- vertices; e <- edgesAdjacentTo(v)) {
      map.getOrElseUpdate(e, ListBuffer[Int]()) += v
    }
    Map() ++ map.mapValues(_.toList)
  }
  lazy val maxEdgeLabel = edgeIncidences.keysIterator.max
  lazy val maxFaceLabel = vertexFlags.iterator.flatMap(_.map(_._2)).max

  def target(source: Int, edge: Int): Int = edgeIncidences(edge).find(_ != source).get
  def faceOnRight(source: Int, edge: Int): Int = vertexFlags(target(source, edge)).find(_._1 == edge).get._2

  val outerFace = vertexFlags(0).last._2

  def boundaryFaces = vertexFlags(0).map(_._2)

  lazy val nautyGraph: ColouredGraph[Int] = {
    val adjacencies = for (v <- vertices) yield {
      for (e <- edgesAdjacentTo(v)) yield {
        target(v, e)
      }
    }
    ColouredGraph(numberOfVertices, adjacencies, vertexLabels)
  }

  case class Subgraphs(shape: PlanarGraph) {
    case class Excision(cut: PlanarGraph, depth: Int, totalRotations: Map[Int, Int]) {
      def replace(other: PlanarGraph): PlanarGraph = ???
    }

    def excisions: Iterator[Excision] = ???
  }
}

object PlanarGraph {
  private def spider = implicitly[Spider[PlanarGraph]]

  def star(k: Int, label: Int) = {
    import net.tqft.toolkit.arithmetic.Mod._

    val labels: IndexedSeq[Int] = IndexedSeq(-1, label)
    val flags = IndexedSeq(
      Seq.tabulate(k)(i => (i, i + 1 mod k)),
      Seq.tabulate(k)(i => (i, i)))
    PlanarGraph(k, labels, flags)
  }

  def I(labelTop: Int, labelBottom: Int) = spider.multiply(star(3, labelTop), star(3, labelBottom), 1)
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
  implicit def graphSpider: Spider[PlanarGraph] = {
    new Spider[PlanarGraph] {
      override def circumference(graph: PlanarGraph) = graph.numberOfBoundaryPoints
      override def rotate(graph: PlanarGraph, k: Int) = {
        import net.tqft.toolkit.collections.Rotate._
        graph.copy(vertexFlags = graph.vertexFlags.updated(0, graph.vertexFlags(0).rotateLeft(k)))
      }
      override def tensor(graph1: PlanarGraph, graph2: PlanarGraph) = {
        def labels = graph1.vertexLabels ++ graph2.vertexLabels.tail
        def flags = {
          val F = graph2.outerFace
          val ne = graph1.maxEdgeLabel
          val nf = graph1.maxFaceLabel
          val relabelFlag: ((Int, Int)) => (Int, Int) = {
            case (e, F) => (e + 1 + ne, graph1.outerFace)
            case (e, f) => (e + 1 + ne, f + 1 + nf)
          }
          val externalFlag = {
            graph1.vertexFlags.head ++ graph2.vertexFlags.head.map(relabelFlag)
          }
          (externalFlag +: graph1.vertexFlags.tail) ++ graph2.vertexFlags.tail.map(_.map(relabelFlag))
        }
        PlanarGraph(graph1.numberOfBoundaryPoints + graph2.numberOfBoundaryPoints, labels, flags)
      }
      override def stitch(graph: PlanarGraph) = {
        val f0 = graph.outerFace
        val f1 = graph.vertexFlags(0)(1)._2
        val nf = if (f0 < f1) f0 else f1
        def relabelFace(f: Int) = {
          if (f == f0 || f == f1) {
            nf
          } else {
            f
          }
        }
        def flags = ???
        PlanarGraph(graph.numberOfBoundaryPoints - 2, graph.vertexLabels, flags)
      }

      override def canonicalForm(graph: PlanarGraph) = {
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

trait LinearSpider[R, M] extends Spider[M] with Module[R, M] {
  def ring: Ring[R]
}

object LinearSpider {
  def rationalSpider = implicitly[LinearSpider[RationalFunction[Int], Disk[Map[PlanarGraph, RationalFunction[Int]]]]]

  class MapLinearSpider[A: Spider, R: Ring] extends LinearSpider[R, Map[A, R]] {
    val diagramSpider = implicitly[Spider[A]]
    override val ring = implicitly[Ring[R]]

    private def mapKeys(f: A => A)(map: TraversableOnce[(A, R)]) = {
      val newMap = scala.collection.mutable.Map[A, R]()
      for ((a, r) <- map) {
        val s = f(a)
        newMap(s) = newMap.get(s).map(v => ring.add(v, r)).getOrElse(r)
      }
      Map() ++ newMap.filter(_._2 != ring.zero)
    }

    override def rotate(map: Map[A, R], k: Int) = map.map(p => (diagramSpider.rotate(p._1, k), p._2))
    override def tensor(map1: Map[A, R], map2: Map[A, R]) = {
      val newMap = scala.collection.mutable.Map[A, R]()
      for ((a, r) <- map1; (b, s) <- map2) {
        val t = diagramSpider.tensor(a, b)
        val p = ring.multiply(r, s)
        newMap(t) = newMap.get(t).map(v => ring.add(v, p)).getOrElse(p)
      }
      Map() ++ newMap.filter(_._2 != ring.zero)
    }
    override def stitch(map: Map[A, R]) = mapKeys(diagramSpider.stitch)(map)
    override def canonicalForm(map: Map[A, R]) = mapKeys(diagramSpider.canonicalForm)(map)
    override def circumference(map: Map[A, R]) = diagramSpider.circumference(map.head._1)

    override def zero = Map.empty
    override def add(map1: Map[A, R], map2: Map[A, R]) = mapKeys(x => x)(map1.iterator ++ map2.iterator)
    override def scalarMultiply(r: R, map: Map[A, R]) = map.mapValues(v => ring.multiply(r, v)).filter(_._2 != ring.zero)
    override def negate(map: Map[A, R]) = map.mapValues(v => ring.negate(v))
  }

  implicit def lift[A: Spider, R: Ring]: LinearSpider[R, Map[A, R]] = new MapLinearSpider[A, R]

  implicit def diskLinearSpider[A, R, M](implicit spider: LinearSpider[R, M]): LinearSpider[R, Disk[M]] = new Spider.DiskSpider(spider) with LinearSpider[R, Disk[M]] {
    override def ring = spider.ring
    override def zero = ???
    override def add(disk1: Disk[M], disk2: Disk[M]) = Disk(disk1.circumference, spider.add(disk1.contents, disk2.contents))
    override def scalarMultiply(r: R, disk: Disk[M]) = Disk(disk.circumference, spider.scalarMultiply(r, disk.contents))
    override def negate(disk: Disk[M]) = Disk(disk.circumference, spider.negate(disk.contents))
  }
}

case class Reduction[A, R](big: A, small: Map[A, R])

trait SubstitutionSpider[A, R] extends LinearSpider.MapLinearSpider[A, R] {
  def eigenvalue: Int => R

  def allReplacements(reduction: Reduction[A, R])(diagram: A): Iterator[Map[A, R]]
  def replace(reduction: Reduction[A, R])(element: Map[A, R]): Map[A, R] = {
    val newMap = scala.collection.mutable.Map[A, R]()
    for ((a, r) <- element) {
      import net.tqft.toolkit.collections.Iterators._
      val m: Map[A, R] = allReplacements(reduction)(a).headOption.getOrElse(Map(a -> ring.one))
      for ((b, t) <- m) {
        val p = ring.multiply(r, t)
        newMap(b) = newMap.get(b).map(v => ring.add(v, p)).getOrElse(p)
      }
    }
    Map() ++ newMap.filter(_._2 != ring.zero)
  }

  def replace(reductions: Seq[Reduction[A, R]])(element: Map[A, R]): Map[A, R] = {
    reductions.iterator.map(r => replace(r)(element)).find(_ != element).getOrElse(element)
  }
  def replaceRepeatedly(reductions: Seq[Reduction[A, R]])(element: Map[A, R]) = {
    import net.tqft.toolkit.functions.FixedPoint._
    (replace(reductions) _).fixedPoint(element)
  }
}

object SubstitutionSpider {
  abstract class PlanarGraphMapSubstitutionSpider[R: Ring] extends LinearSpider.MapLinearSpider[PlanarGraph, R] with SubstitutionSpider[PlanarGraph, R] {
    def eigenvalue: Int => R
    override def allReplacements(reduction: Reduction[PlanarGraph, R])(diagram: PlanarGraph) = {
      for (excision <- diagram.Subgraphs(reduction.big).excisions) yield {
        val eigenvalueFactor = ring.product(excision.totalRotations.map({
          case (label, p) => ring.power(eigenvalue(label), p)
        }))
        val newMap = scala.collection.mutable.Map[PlanarGraph, R]()
        for ((a, r) <- reduction.small) {
          val b = diagramSpider.canonicalForm(excision.replace(a))
          val p = ring.multiply(eigenvalueFactor, r)
          newMap(b) = newMap.get(b).map(v => ring.add(v, p)).getOrElse(p)
        }
        Map() ++ newMap.filter(_._2 != ring.zero)
      }
    }
  }

  def withEigenvalues[R: Ring](eigenvalue: Int => R): SubstitutionSpider[PlanarGraph, R] = {
    def eigenvalue_ = eigenvalue
    new PlanarGraphMapSubstitutionSpider {
      override val eigenvalue = eigenvalue_
    }
  }
}

trait ReductionSpider[A, R] extends SubstitutionSpider[A, R] {
  def reductions: Seq[Reduction[A, R]]
  override def canonicalForm(m: Map[A, R]) = super.canonicalForm(replaceRepeatedly(reductions)(m))
}

abstract class PlanarGraphReductionSpider[R: Ring] extends SubstitutionSpider.PlanarGraphMapSubstitutionSpider[R] with ReductionSpider[PlanarGraph, R]

abstract class TrivalentSpider[R: Ring] extends PlanarGraphReductionSpider[R] {
  def d: R
  def b: R
  def t: R
  def omega: R
  override val eigenvalue = { x: Int => omega }
  override val reductions = ???
}


object `SO(3)_q` extends TrivalentSpider[RationalFunction[Int]] {
  override val ring = implicitly[Field[RationalFunction[Int]]]
  
  val q: RationalFunction[Int] = Polynomial(1 -> Fraction(1, 1))
  override val omega = (1: RationalFunction[Int])
  override val d = ring.add(q, ring.inverse(q))
  override val b = ???
  override val t = ???
  
  override val reductions = ???
}
