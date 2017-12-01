package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.orderings.Orderings
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters

object PolyhedronNamer {
  // global mutable state, oh my!
  val names = {
    import JavaConverters._
    new ConcurrentHashMap[PlanarGraph, String]().asScala
  }

  def byName(name: String) = names.find(_._2 == name).map(_._1)

  private var count = 0
  private def nextName = {
    count = count + 1
    "p" + count
  }

}

trait PolyhedronNamer[A, F] extends FunctionSpider[A, F] {

  protected def variableToPolynomial(v: String): F

  def polyhedronReductions = PolyhedronNamer.names.toSeq.flatMap({ p =>
    sphericalEquivalents(p._1).map({ q =>
//      require(q._2.vertexRotations.isEmpty)
      Reduction[PlanarGraph, F](q._1, Map(PlanarGraph.empty -> ring.multiply(variableToPolynomial(p._2), eigenvalue(q._2.reverse))))
    })
  })

  val polyhedronOrdering: Ordering[String] = {
    // TODO hack
    import Orderings._
    val o = Ordering.by({ s: String =>
      PolyhedronNamer.names.find(_._2 == s).map(p => p._1.numberOfInternalVertices).getOrElse(s match { case "d" => 0; case "b" => 2 })
    }).refineBy(s => s)
    new Ordering[String] {
      override def toString = "polyhedronOrdering: " + (PolyhedronNamer.names.values.toSeq ++ Seq("b", "d")).sorted(o).mkString(" < ")
      override def compare(x: String, y: String) = o.compare(x, y)
    }
  }

  private def sphericalEquivalents_(p: PlanarGraph) = {
    require(p.numberOfBoundaryPoints == 0)
    p.faceSet.toSeq.map(f => diagramSpider.canonicalFormWithDefect(p.copy(outerFace = f))).distinct
  }

  val sphericalEquivalents = {
    import net.tqft.toolkit.functions.Memo
    Memo(sphericalEquivalents_ _)
  }

  def sphericalCanonicalForm(p: PlanarGraph) = {
    sphericalEquivalents(p).sortBy(_._1).head
  }

  override def evaluate(map: Map[PlanarGraph, F]): F = {
    ring.sum(for ((k, v) <- map) yield {
      val ck = diagramSpider.canonicalFormWithDefect(k)
      if (ck._1 == diagramSpider.empty) {
        require(ck._2.vertexRotations.isEmpty)
        v
      } else {
//        val k0 = if (k.loops == 0) { k } else { k.copy(loops = 0) }
        val (sck, rot) = sphericalCanonicalForm(k)
        val name = PolyhedronNamer.names.get(sck) match {
          case Some(name) => name
          case None => {
            val newName = PolyhedronNamer.nextName
            println("Naming new polyhedron:")
            println(ck)
            println(newName)
            PolyhedronNamer.names += (sck -> newName)
            newName
          }
        }
        ring.multiply(ring.multiply(v, variableToPolynomial(name)), eigenvalue(rot))
      }
    })
  }
}

trait PolynomialPolyhedronNamer[A] extends PolyhedronNamer[A, MultivariablePolynomial[A, String]] {
  override def variableToPolynomial(v: String) = MultivariablePolynomial(Map(Map(v -> 1) -> coefficientRing.one))
}
trait RationalFunctionPolyhedronNamer[A] extends PolyhedronNamer[A, MultivariableRationalFunction[A, String]] {
  override def variableToPolynomial(v: String) = MultivariablePolynomial(Map(Map(v -> 1) -> coefficientRing.one))
}
trait RationalExpressionPolyhedronNamer[A] extends PolyhedronNamer[A, RationalExpression[A, String]] {
  override def variableToPolynomial(v: String) = RationalExpression.variable(v)
}
