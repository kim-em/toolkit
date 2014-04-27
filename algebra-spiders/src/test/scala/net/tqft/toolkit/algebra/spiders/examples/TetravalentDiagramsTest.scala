package net.tqft.toolkit.algebra.spiders.examples

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import net.tqft.toolkit.algebra.spiders._

@RunWith(classOf[JUnitRunner])
class TetravalentDiagramsTest extends FlatSpec with Matchers with IsomorphismMatchers {
  val tetravalentEnumerator = GraphsGeneratedBy(Seq((4, 1)))
  val tetravalentDiagrams = tetravalentEnumerator.avoiding(TetravalentSpider.reductions.map(_.big))

  "byNumberOfVertices" should "find 1 diagrams with 0 boundary points and 0 vertices" in {
    tetravalentDiagrams.byNumberOfVertices(0, Map.empty[VertexType, Int]).size should equal(1)
  }
  "byNumberOfVertices" should "find 0 diagrams with 0 boundary points and 1 vertices" in {
    tetravalentDiagrams.byNumberOfVertices(0, Map(VertexType(4, 1) -> 1)).size should equal(0)
  }
  "byNumberOfVertices" should "find 1 diagrams with 0 boundary points and 2 vertices" in {
    tetravalentDiagrams.byNumberOfVertices(0, Map(VertexType(4, 1) -> 2)).size should equal(1)
  }
  "byNumberOfVertices" should "find 4 diagrams with 0 boundary points and 3 vertices" in {
//    for(d <- tetravalentDiagrams.byNumberOfVertices(0, Map(VertexType(4, 1) -> 3))) println(d)
    tetravalentDiagrams.byNumberOfVertices(0, Map(VertexType(4, 1) -> 3)).size should equal(4)
  }
  
  "byNumberOfVertices" should "find 2 diagrams with 4 boundary points and 0 vertices" in {
    tetravalentDiagrams.byNumberOfVertices(4, Map.empty[VertexType, Int]).size should equal(2)
  }
  "byNumberOfVertices" should "find 1 diagrams with 4 boundary points and 1 vertices" in {
    tetravalentDiagrams.byNumberOfVertices(4, Map(VertexType(4, 1) -> 1)).size should equal(1)
  }
  "byNumberOfVertices" should "find 0 diagrams with 4 boundary points and 2 vertices" in {
    tetravalentDiagrams.byNumberOfVertices(4, Map(VertexType(4, 1) -> 2)).size should equal(0)
  }
  "byNumberOfVertices" should "find 5 diagrams with 6 boundary points and 0 crossing" in {
    tetravalentDiagrams.byNumberOfVertices(6, Map(VertexType(4, 1) -> 0)).size should equal(5)
  }
  "byNumberOfVertices" should "find 6 diagrams with 6 boundary points and 1 crossings" in {
    tetravalentDiagrams.byNumberOfVertices(6, Map(VertexType(4, 1) -> 1)).size should equal(6)
  }
  "byNumberOfVertices" should "find 3 diagrams with 6 boundary points and 2 crossings" in {
    tetravalentDiagrams.byNumberOfVertices(6, Map(VertexType(4, 1) -> 2)).size should equal(3)
  }
  "byNumberOfVertices" should "find 2 diagrams with 6 boundary points and 3 crossings" in {
    tetravalentDiagrams.byNumberOfVertices(6, Map(VertexType(4, 1) -> 3)).size should equal(2)
  }
  "byNumberOfVertices" should "find 14 diagrams with 8 boundary points and 0 crossings" in {
    TetravalentSpider.reducedDiagrams(8, 0).size should equal(14)
  }
  "byNumberOfVertices" should "find 28 diagrams with 8 boundary points and 1 crossings" in {
    TetravalentSpider.reducedDiagrams(8, 1).size should equal(28)
  }
}