package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.spiders.LinearSpider
import net.tqft.toolkit.algebra.spiders.EvaluableSpider
import net.tqft.toolkit.algebra.spiders.PlanarGraph

object TrivalentBraidings {

  def preparePiRotationEquation[R, M](spider: LinearSpider[R, M], crossing: M): M = {
    spider.subtract(crossing, spider.rotate(crossing, 2))
  }
  def prepareR2Equation[R, M](spider: LinearSpider[R, M], crossing: M): M = {
    spider.subtract(spider.multiply(crossing, spider.rotate(crossing, 1), 2), spider.rotate(spider.tensor(spider.strand, spider.strand), 1))
  }

  def prepareFourBoxEquations[R, M](spider: LinearSpider[R, M], crossing: M): Seq[M] = {
    Seq(
      preparePiRotationEquation(spider, crossing),
      prepareR2Equation(spider, crossing))
  }

  def prepareR4aEquation[R, M](spider: LinearSpider[R, M], trivalentVertex: M, crossing: M): M = {
    spider.subtract(
      spider.multiply(trivalentVertex, crossing, 1),
      spider.rotate(spider.multiply(
        spider.rotate(spider.multiply(crossing, spider.rotate(crossing, 1), 1), -1),
        trivalentVertex, 2), -1))
  }
  def prepareR4bEquation[R, M](spider: LinearSpider[R, M], trivalentVertex: M, crossing: M): M = {
    prepareR4aEquation(spider, trivalentVertex, spider.rotate(crossing, 1))
  }

  def prepareFiveBoxEquations[R, M](spider: LinearSpider[R, M], trivalentVertex: M, crossing: M): Seq[M] = {
    Seq(
      prepareR4aEquation(spider, trivalentVertex, crossing),
      prepareR4bEquation(spider, trivalentVertex, crossing))
  }

  def prepareBraidingEquations[R, M](spider: LinearSpider[R, M], trivalentVertex: M, crossing: M): Seq[M] = {
    prepareFourBoxEquations(spider, crossing) ++ prepareFiveBoxEquations(spider, trivalentVertex, crossing)
  }

  def prepareBraidingEquationInnerProducts[R](spider: LinearSpider.MapLinearSpider[PlanarGraph, R], trivalentVertex: Map[PlanarGraph, R], crossing: Map[PlanarGraph, R], fourBoxSpanningSet: Seq[Map[PlanarGraph, R]], fiveBoxSpanningSet: Seq[Map[PlanarGraph, R]]): Seq[R] = {
    (for (r <- prepareFourBoxEquations(spider, crossing); d <- fourBoxSpanningSet) yield {
      spider.evaluatedInnerProduct(r, d)
    }) ++
      (for (r <- prepareFiveBoxEquations(spider, trivalentVertex, crossing); d <- fiveBoxSpanningSet) yield {
        spider.evaluatedInnerProduct(r, d)
      })
  }
}