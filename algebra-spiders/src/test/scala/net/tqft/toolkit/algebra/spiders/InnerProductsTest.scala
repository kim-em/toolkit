package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class InnerProductsTest extends FlatSpec with Matchers with IsomorphismMatchers {
  val spider = Trivalent.TrivalentSpider

  "inner products of D(4,0)" should "be correct" in {
    val diagrams = TrivalentGraphs.withoutSmallFaces(4,4,0) ++ TrivalentGraphs.withoutSmallFaces(4,4,2)
    val matrix = for(x <- diagrams) yield {
      for(y <- diagrams) yield {
        spider.innerProduct(Map(x -> 1), Map(y -> 1))
      }
    }
    println(matrix)
  }
  
}

