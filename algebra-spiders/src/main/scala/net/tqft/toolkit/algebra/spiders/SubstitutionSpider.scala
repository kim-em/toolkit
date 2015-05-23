package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.Ring

case class Reduction[A, R](big: A, small: Map[A, R])

trait SubstitutionSpider[A, R] extends LinearSpider.MapLinearSpider[A, R] {
  def eigenvalue(valence: Int): R

  def allReplacements(reduction: Reduction[A, R])(diagram: A): Iterator[Map[A, R]]
  def replace(reduction: Reduction[A, R])(element: Map[A, R]): Map[A, R] = {
    val newMap = scala.collection.mutable.Map[A, R]()
    for ((a, r) <- element) {
      import net.tqft.toolkit.collections.Iterators._
      val m: Map[A, R] = allReplacements(reduction)(a).headOption match {
        case Some(replacement) => {
//          pw.println("Found a replacement:")
//          pw.println("reduction = " + reduction)
//          pw.println("a = " + a)
//          pw.flush
          replacement
        }
        case None => Map(a -> ring.one)
      }
      for ((b, t) <- m) {
        val p = ring.multiply(r, t)
        newMap(b) = newMap.get(b).map(v => ring.add(v, p)).getOrElse(p)
      }
    }
    Map() ++ newMap.filter(x => !ring.zero_?(x._2))
  }

  def replace(reductions: Seq[Reduction[A, R]])(element: Map[A, R]): Map[A, R] = {
    reductions.iterator.map(r => replace(r)(element)).find(_ != element).getOrElse(element)
  }
  def replaceRepeatedly(reductions: Seq[Reduction[A, R]])(element: Map[A, R]) = {
    import net.tqft.toolkit.functions.FixedPoint._
    (replace(reductions) _).fixedPoint(element)
  }
  def allReplacements(reductions: Seq[Reduction[A, R]])(element: Map[A, R]): Iterator[Map[A, R]] = {
    reductions.iterator.map(r => replace(r)(element)).filter(_ != element)
  }
  def allReplacementsRepeated(reductions: Seq[Reduction[A, R]])(element: Map[A, R]): Iterator[Map[A, R]] = {
    Iterator(element) ++ allReplacements(reductions)(element).flatMap(allReplacementsRepeated(reductions))
  }
}

object SubstitutionSpider {
  abstract class PlanarGraphMapSubstitutionSpider[R: Ring] extends LinearSpider.MapLinearSpider[PlanarGraph, R] with SubstitutionSpider[PlanarGraph, R] with CachingEvaluableSpider[R, Map[PlanarGraph, R]] {
    def vertexTypes: Seq[VertexType]
    val graphs = GraphsGeneratedBy(vertexTypes)

    override def allReplacements(reduction: Reduction[PlanarGraph, R])(diagram: PlanarGraph) = {
      for (
        excision <- diagram.Subgraphs(reduction.big).excisions
      ) yield {
        val eigenvalueFactor1 = eigenvalue(excision.rotations)
        val newMap = scala.collection.mutable.Map[PlanarGraph, R]()
        for ((a, r) <- reduction.small) {
          val (b, rotations2) = diagramSpider.canonicalFormWithDefect(excision.replace(a))
          //          val br = diagramSpider.rotate(b, -rotations2.boundaryRotation)
          val eigenvalueFactor2 = eigenvalue(rotations2)
          val p = ring.multiply(eigenvalueFactor1, eigenvalueFactor2, r)
          newMap(b) = newMap.get(b).map(v => ring.add(v, p)).getOrElse(p)
        }
        Map() ++ newMap.filter(x => !ring.zero_?(x._2))
      }
    }
  }
}