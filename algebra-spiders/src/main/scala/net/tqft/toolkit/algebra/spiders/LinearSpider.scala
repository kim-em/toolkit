package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra._

trait CanonicalLabelling[A] {
  def canonicalForm(a: A): A
}

trait LinearSpider[R, M] extends Spider[M] with CanonicalLabelling[M] with Module[R, M] {
  def eigenvalue(valence: Int): R
  def eigenvalue(rotations: Rotation): R = {
    ring.product(rotations.vertexRotations.map({ case (v, p) => ring.power(eigenvalue(v), p) }))
  }
  def ring: Ring[R]
  override def innerProduct(a: M, b: M) = canonicalForm(super.innerProduct(a, b))
}

trait EvaluableSpider[R, A] extends Spider[A] {
  def evaluate(a: A): R
  def evaluatedInnerProduct(a1: A, a2: A) = evaluate(innerProduct(a1, a2))
}

trait CachingEvaluableSpider[R, A] extends EvaluableSpider[R, A] {
  private val cache = scala.collection.mutable.Map[(A, A), R]()
  override def evaluatedInnerProduct(a1: A, a2: A) = cache.getOrElseUpdate((a1, a2), super.evaluatedInnerProduct(a1, a2))
}

object LinearSpider {
  abstract class MapLinearSpider[A: DiagramSpider, R: Ring] extends Module.ModuleMap[R, A, R] with LinearSpider[R, Map[A, R]] with EvaluableSpider[R, Map[A, R]] {
    val diagramSpider = implicitly[DiagramSpider[A]]
    override lazy val coefficients = Module.moduleOverItself(ring)

    override def empty = Map(diagramSpider.empty -> implicitly[Ring[R]].one)

    private def mapKeys(f: A => A)(map: TraversableOnce[(A, R)]) = {
      val newMap = scala.collection.mutable.Map[A, R]()
      for ((a, r) <- map) {
        val s = f(a)
        newMap(s) = newMap.get(s).map(v => ring.add(v, r)).getOrElse(r)
      }
      canonicalForm(Map() ++ newMap.filter(x => !ring.zero_?(x._2)))
    }

    override def rotate(map: Map[A, R], k: Int) = mapKeys(d => diagramSpider.rotate(d, k))(map) 
    override def stitch(map: Map[A, R]) = mapKeys(diagramSpider.stitch)(map)
    override def tensor(map1: Map[A, R], map2: Map[A, R]) = {
      val newMap = scala.collection.mutable.Map[A, R]()
      for ((a, r) <- map1; (b, s) <- map2) {
        val t = diagramSpider.tensor(a, b)
        val p = ring.multiply(r, s)
        newMap(t) = newMap.get(t).map(v => ring.add(v, p)).getOrElse(p)
      }
      canonicalForm(Map() ++ newMap.filter(x => !ring.zero_?(x._2)))
    }
    override def canonicalForm(map: Map[A, R]) = {
      val newMap = scala.collection.mutable.Map[A, R]()
      for ((a, r) <- map) {
        val (b, rotations) = diagramSpider.canonicalFormWithDefect(a)
        val p = ring.multiply(r, eigenvalue(rotations))
        newMap(b) = newMap.get(b).map(v => ring.add(v, p)).getOrElse(p)
      }
      Map() ++ newMap.filter(x => !ring.zero_?(x._2))
    }

    override def circumference(map: Map[A, R]) = diagramSpider.circumference(map.head._1)

    override def evaluate(map: Map[A, R]) = {
      if (map.isEmpty) {
        ring.zero
      } else {
        if (map.size > 1) {
          throw new UnsupportedOperationException("No default partition function for " + map)
        } else {
          val (k, v) = map.head
          if (diagramSpider.canonicalFormWithDefect(k)._1 == diagramSpider.empty) {
            v
          } else {
            throw new UnsupportedOperationException("No default partition function for " + map)
          }
        }
      }
    }
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