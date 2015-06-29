package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.Ring

case class Reduction[A, R](big: A, small: Map[A, R])

trait SubstitutionSpider[A, R] extends LinearSpider.MapLinearSpider[A, R] {
  def eigenvalue(valence: Int): R

  def allDiagramReplacements(reduction: Reduction[A, R])(diagram: A): Iterator[Map[A, R]]
  def allDiagramReplacements(reductions: Seq[Reduction[A, R]])(diagram: A): Iterator[Map[A, R]] = {
    reductions.iterator.flatMap(r => allDiagramReplacements(r)(diagram))
  }

  lazy val cachedDiagramReplacementOption = {
    import net.tqft.toolkit.functions.Memo
    Memo({ reductions: Seq[Reduction[A, R]] =>
      Memo.softly({ diagram: A =>
        import net.tqft.toolkit.collections.Iterators._
        allDiagramReplacements(reductions)(diagram).headOption
      })
    })
  }

  def replace(reductions: Seq[Reduction[A, R]])(element: Map[A, R]): Option[Map[A, R]] = {
    val newMap = scala.collection.mutable.Map[A, R]()
    var touched = false
    for ((a, r) <- element) {
      import net.tqft.toolkit.collections.Iterators._
      allDiagramReplacements(reductions)(a).headOption match {
        case Some(replacement) => {
          touched = true
          for ((b, t) <- replacement) {
            val p = ring.multiply(r, t)
            newMap(b) = newMap.get(b).map(v => ring.add(v, p)).getOrElse(p)
          }
        }
        case None => {
          newMap(a) = newMap.get(a).map(v => ring.add(v, r)).getOrElse(r)
        }
      }
    }
    if (touched) {
      Some(Map() ++ newMap.filter(x => !ring.zero_?(x._2)))
    } else {
      None
    }
  }

  def replaceRepeatedly(reductions: Seq[Reduction[A, R]])(element: Map[A, R]): Map[A, R] = {
    val stack = scala.collection.mutable.Stack[(A, R)]()
    
    def putSeveralOnStack(pairs: Traversable[(A, R)]) {
      for((a,r) <- pairs) putOnStack(a, r)
    }
    def putOnStack(a: A, r: R) {
      stack.indexWhere(_._1 == a) match {
        case -1 => stack.push((a, r))
        case k => stack.updated(k, (a, ring.add(stack.apply(k)._2, r)))
      }
    }
    
    var touched = false
    stack.pushAll(element)
    val done = scala.collection.mutable.Map[A, R]()
    while (stack.nonEmpty) {
      val (a, r) = stack.pop
      cachedDiagramReplacementOption(reductions)(a) match {
        case None => done(a) = done.get(a).map(v => ring.add(v, r)).getOrElse(r)
        case Some(map) => {
          touched = true
          putSeveralOnStack(map.mapValues(v => ring.multiply(v, r)))
        }
      }
    }
    if (touched) {
      Map() ++ done.filter(x => !ring.zero_?(x._2))
    } else {
      element
    }
  }
  
  
//  def allReplacements(reductions: Seq[Reduction[A, R]])(element: Map[A, R]): Iterator[Map[A, R]] = {
//    reductions.iterator.map(r => replace(r)(element)).collect({ case Some(result) => result })
//  }
//  def allReplacementsRepeated(reductions: Seq[Reduction[A, R]])(element: Map[A, R]): Iterator[Map[A, R]] = {
//    Iterator(element) ++ allReplacements(reductions)(element).flatMap(allReplacementsRepeated(reductions))
//  }
}

object SubstitutionSpider {
  abstract class PlanarGraphMapSubstitutionSpider[R: Ring] extends LinearSpider.MapLinearSpider[PlanarGraph, R] with SubstitutionSpider[PlanarGraph, R] with CachingEvaluableSpider[R, Map[PlanarGraph, R]] {
    def vertexTypes: Seq[VertexType]
    val graphs = GraphsGeneratedBy(vertexTypes)

    override def allDiagramReplacements(reduction: Reduction[PlanarGraph, R])(diagram: PlanarGraph) = {
      for (
        excision <- diagram.subgraphs(reduction.big).cachedExcisions.iterator
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