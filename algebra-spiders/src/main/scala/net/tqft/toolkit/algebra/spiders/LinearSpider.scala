package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.Module

trait CanonicalLabelling[A] {
  def canonicalForm(a: A): A
}


trait LinearSpider[R, M] extends Spider[M] with CanonicalLabelling[M] with Module[R, M] {
  def eigenvalue(valence: Int): R
  def eigenvalue(rotations: Rotation): R = {
    ring.product(rotations.vertexRotations.map({ case (v, p) => ring.power(eigenvalue(v), p) }))
  }
  def ring: Ring[R]
}

object LinearSpider {
  abstract class MapLinearSpider[A: DiagramSpider, R: Ring] extends LinearSpider[R, Map[A, R]] {
    val diagramSpider = implicitly[DiagramSpider[A]]
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
    override def canonicalForm(map: Map[A, R]) = {
      val newMap = scala.collection.mutable.Map[A, R]()
      for ((a, r) <- map) {
        val (b, rotations) = diagramSpider.canonicalFormWithDefect(a)
//        val br = diagramSpider.rotate(b, - rotations.boundaryRotation)
        val p = ring.multiply(r, eigenvalue(rotations))
        newMap(b) = newMap.get(b).map(v => ring.add(v, p)).getOrElse(p)
      }
      Map() ++ newMap.filter(_._2 != ring.zero)
    }

    override def circumference(map: Map[A, R]) = diagramSpider.circumference(map.head._1)

    override def zero = Map.empty
    override def add(map1: Map[A, R], map2: Map[A, R]) = mapKeys(x => x)(map1.iterator ++ map2.iterator)
    override def scalarMultiply(r: R, map: Map[A, R]) = map.mapValues(v => ring.multiply(r, v)).filter(_._2 != ring.zero)
    override def negate(map: Map[A, R]) = map.mapValues(v => ring.negate(v))
  }

  implicit def diskLinearSpider[A, R, M](implicit spider: LinearSpider[R, M]): LinearSpider[R, Disk[M]] = new Spider.DiskSpider(spider) with LinearSpider[R, Disk[M]] {
    override def eigenvalue(valence: Int) = spider.eigenvalue(valence)
    override def ring = spider.ring
    override def zero = ???
    override def add(disk1: Disk[M], disk2: Disk[M]) = Disk(disk1.circumference, spider.add(disk1.contents, disk2.contents))
    override def scalarMultiply(r: R, disk: Disk[M]) = Disk(disk.circumference, spider.scalarMultiply(r, disk.contents))
    override def negate(disk: Disk[M]) = Disk(disk.circumference, spider.negate(disk.contents))
    override def canonicalForm(disk: Disk[M]) = Disk(disk.circumference, spider.canonicalForm(disk.contents))
  }
}