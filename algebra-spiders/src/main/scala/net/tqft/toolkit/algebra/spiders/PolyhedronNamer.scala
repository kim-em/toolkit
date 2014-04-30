package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.polynomials._

trait PolyhedronNamer[A] extends MultivariableRationalFunctionSpider[A] {

  val names = scala.collection.mutable.Map[PlanarGraph, String](
    PlanarGraph(7, IndexedSeq(List(), List((6, 10), (3, 8), (4, 7), (5, 9)), List((3, 7), (6, 8), (5, 10), (4, 9))), List(2, 2), 0) -> "hopf")
  def namedReductions = names.toSeq.flatMap({ p =>
    sphericalEquivalents(p._1).map({ q =>
      Reduction[PlanarGraph, MultivariableRationalFunction[A, String]](q, Map(PlanarGraph.empty -> Map(Map(p._2 -> 1) -> coefficientRing.one)))
    })
  })

  val polyhedronOrdering: Ordering[String] = {
    // TODO hack
    Ordering.by({ s: String => - names.find(_._2 == s ).map(p => p._1.numberOfInternalVertices).getOrElse(s match { case "d" => 0; case "b" => 2 }) })
  }
  
  private var count = 0
  private def nextName = {
    count = count + 1
    "p" + count
  }

  private def sphericalEquivalents_(p: PlanarGraph) = {
    require(p.numberOfBoundaryPoints == 0)
    p.faceSet.toSeq.map(f => diagramSpider.canonicalFormWithDefect(p.copy(outerFace = f))._1)
  }

  val sphericalEquivalents = {
    import net.tqft.toolkit.functions.Memo
    Memo(sphericalEquivalents_ _)
  }

  def sphericalCanonicalForm(p: PlanarGraph) = {
    sphericalEquivalents(p).sorted.head
  }

  override def evaluate(map: Map[PlanarGraph, MultivariableRationalFunction[A, String]]): MultivariableRationalFunction[A, String] = {
    ring.sum(for ((k, v) <- map) yield {
      if (diagramSpider.canonicalFormWithDefect(k)._1 == diagramSpider.empty) {
        v
      } else {
        val ck = sphericalCanonicalForm(k)
        if (!names.keys.exists(p => p == ck)) {
          val newName = nextName
          println("Naming new polyhedron:")
          println(ck)
          println(newName)
          names += (ck -> newName)
        }
        replace(namedReductions)(Map(k -> v))(PlanarGraph.empty)
      }
    })
  }
}
