package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra._
import java.io.FileOutputStream
import java.io.PrintWriter

abstract class PlanarGraphReductionSpider[R: Ring] extends SubstitutionSpider.PlanarGraphMapSubstitutionSpider[R] with ReductionSpider[PlanarGraph, R] {
  // TODO move these further up the hierarchy?

  def innerProductsOfLinearCombinationsMatrix(linearCombos1: Seq[Map[PlanarGraph, R]], linearCombos2: Seq[Map[PlanarGraph, R]]): Seq[Seq[R]] = {
    for (x <- linearCombos1) yield {
      for (y <- linearCombos2) yield {
        evaluatedInnerProduct(x, y)
      }
    }
  }

  //  def allInnerProductEvaluations(d1: PlanarGraph, d2: PlanarGraph): Seq[R] = {
  //    pw.println("computing all inner products of")
  //    pw.println(d1)
  //    pw.println(d2)
  //    pw.println("with reductions")
  //    pw.println(reductions)
  //    pw.flush
  //    allEvaluations(diagramSpider.innerProduct(d1, d2)).toList
  //  }
  //  def allInnerProductEvaluations(m: Map[PlanarGraph, R], d2: PlanarGraph): Seq[R] = {
  //    val terms = m.toSeq.par.map(p => (allInnerProductEvaluations(p._1, d2), p._2)).seq
  //    terms.foldLeft(Seq(ring.zero))({
  //      case (partialSums, (nextTerms, coefficient)) => {
  //        for(x <- partialSums; y <- nextTerms) yield {
  //          ring.add(x, ring.multiply(y, coefficient))
  //        }
  //      } 
  //    })
  //  }
  //  def allInnerProductEvaluations(m: Map[PlanarGraph, R], diagrams: Seq[PlanarGraph]): Seq[R] = {
  //    (for(d <- diagrams.par; r <- allInnerProductEvaluations(m, d)) yield r).seq
  //  }

  def innerProductMatrix(diagrams1: Seq[PlanarGraph], diagrams2: Seq[PlanarGraph]): Seq[Seq[R]] = {
    def ring = implicitly[Ring[R]]

    try {
      val result = (for (x <- diagrams1.par) yield {
        //      println("computing inner products: x = " + x)
        print(".")
        (for (y <- diagrams2) yield {
          //        println("computing inner products: y = " + y)
          //        println("Reductions: ")
          //        for (r <- reductions) println("  " + r)
          //        evaluatedInnerProduct(Map(x -> ring.one), Map(y -> ring.one))
          evaluatedInnerProduct(x, y)
        })
      })
      println("")
      result.seq
    } catch {
      case e: Exception => {
        val pw = new PrintWriter(new FileOutputStream("/Users/scott/err.log"))
        e.printStackTrace(pw)
        pw.flush
        throw e
      }
    }
  }

  def evaluatedInnerProduct(x: PlanarGraph, y: PlanarGraph) = {
    evaluate(replaceRepeatedly(reductions)(Map(diagramSpider.innerProduct(x, y) -> ring.one)))
  }

  def innerProductMatrix(diagrams: Seq[PlanarGraph]): Seq[Seq[R]] = innerProductMatrix(diagrams, diagrams)

  def reducedDiagrams(numberOfBoundaryPoints: Int, numberOfVertices: Int): Seq[PlanarGraph] = {
    require(vertexTypes.size == 1)
    reducedDiagrams(numberOfBoundaryPoints, Map(vertexTypes.head -> numberOfVertices))
  }
  def reducedDiagrams(numberOfBoundaryPoints: Int, numberOfVertices: Map[VertexType, Int]): Seq[PlanarGraph] = {
    graphs.avoiding(reductions.map(_.big)).byNumberOfVertices(numberOfBoundaryPoints, numberOfVertices)
  }
}
