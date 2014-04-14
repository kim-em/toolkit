package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class ExceptionalDiagramsTest extends FlatSpec with Matchers with IsomorphismMatchers {
  val exceptionalEnumerator = GraphsGeneratedBy(Seq((3, 1), (4, 2)))
  val exceptionalDiagrams = exceptionalEnumerator.avoiding(QuantumExceptional.reductions.map(_.big))

  "byNumberOfVertices" should "find 1 diagrams with 0 boundary points and 0 vertices" in {
    exceptionalDiagrams.byNumberOfVertices(0, Map.empty[VertexType, Int]).size should equal(1)
  }
  "byNumberOfVertices" should "find 2 diagrams with 4 boundary points and 0 vertices" in {
    exceptionalDiagrams.byNumberOfVertices(4, Map.empty[VertexType, Int]).size should equal(2)
  }
  "byNumberOfVertices" should "find 2 diagrams with 4 boundary points and 2 vertices" in {
    exceptionalDiagrams.byNumberOfVertices(4, Map(VertexType(3,1)->2)).size should equal(2)
  }
  "byNumberOfVertices" should "find 2 diagrams with 4 boundary points and 4 vertices" in {
    exceptionalDiagrams.byNumberOfVertices(4, Map(VertexType(3,1)->4)).size should equal(1)
  }
  "byNumberOfVertices" should "find 2 diagrams with 4 boundary points and 1 crossing" in {
    exceptionalDiagrams.byNumberOfVertices(4, Map(VertexType(4,2)->1)).size should equal(2)
  }
  "byNumberOfVertices" should "find 4 diagrams with 4 boundary points and 2 crossing" in {
    exceptionalDiagrams.byNumberOfVertices(4, Map(VertexType(4,2)->2)).size should equal(4)
  }

}