package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import net.tqft.toolkit.Logging

import net.tqft.toolkit.arithmetic.Mod._

case class PlanarGraphEnumerationContext(vertices: Seq[VertexType]) extends Logging {
  val spider = PlanarGraph.spider

  case class PlanarGraphEnumeration(G: PlanarGraph) extends CanonicalGeneration[PlanarGraphEnumeration, Unit] { pge =>
    val newLabel = (0 +: G.labels.map(_._1)).max + 1

    override lazy val automorphisms: FinitelyGeneratedFiniteGroup[Unit] = FinitelyGeneratedFiniteGroup.trivialGroup(())

    case class Upper(whereToStart: Int, vertexToAdd: VertexType, vertexRotation: Int, numberOfStitches: Int, basepointOffset: Option[Int]) {
      require(basepointOffset.nonEmpty == (numberOfStitches >= whereToStart))
      
      lazy val result = {
        val vertex = PlanarGraph.star(vertexToAdd)
        PlanarGraphEnumeration(
          spider.rotate(
            spider.multiply(spider.rotate(G, -whereToStart), spider.rotate(vertex, -vertexRotation), numberOfStitches),
            basepointOffset match {
              case None => -G.numberOfBoundaryPoints + whereToStart // because numberOfStitches < whereToStart, these strings are still there!
              case Some(r) => vertexToAdd.perimeter - numberOfStitches - r // rotate all the new strings back to the left, then move r strings back to the right
            }).copy(comment = Some(s"Upper(wheretoStart = $whereToStart, numberOfStitches = $numberOfStitches, basepointOffset = $basepointOffset) boundaryInterval = $boundaryInterval")))
      }
      // boundaryInterval should be the most clockwise position we can see the new vertex from
      // we count clockwise!
      val boundaryInterval = (basepointOffset match {
          case None    => whereToStart + vertexToAdd.perimeter - 2 * numberOfStitches
          case Some(r) => vertexToAdd.perimeter - numberOfStitches - r  
        }) mod (G.numberOfBoundaryPoints + vertexToAdd.perimeter - 2 * numberOfStitches)
      def inverse = {        
        result.Lower(boundaryInterval, G.numberOfInternalVertices + 1)
      }
    }
    case class Lower(boundaryInterval: Int, vertexToRemove: Int) {
      require(vertexToRemove != 0)
      // TODO understand the rule about _which_ boundaryIntervals are allowed,
      // and enforce it here

      private lazy val relabeled = G.copy(labels = G.labels.updated(vertexToRemove - 1, (newLabel, G.labels(vertexToRemove - 1)._2)))

      lazy val result = ???
      //      {
      //        val rotated = spider.rotate(relabeled, boundaryInterval /* negative?! */ )
      //        
      //        DrawPlanarGraph.showPDF(rotated)
      //        
      //        val excisions = rotated.Subgraphs(PlanarGraph.star(G.vertexFlags(vertexToRemove).size, newLabel, G.labels(vertexToRemove - 1)._2)).excisions
      //        
      //        val result = excisions.next
      //        require(result.depth == 0)
      //        require(!excisions.hasNext)
      //        
      //        PlanarGraphEnumeration(result.cut)
      //      }

      def encodeAsPlanarGraph: PlanarGraph = {
        val markerVertex = PlanarGraph.star(VertexType(1, 1, 1))
        spider.rotate(spider.tensor(markerVertex, spider.rotate(relabeled, -boundaryInterval)), boundaryInterval).relabelEdgesAndFaces
      }
    }

    override def upperObjects: automorphisms.ActionOnFiniteSet[Upper] = new automorphisms.ActionOnFiniteSet[Upper] {
      override def elements = {
        val elementsThatDontConnectToFirstString =
          for (
            vertexToAdd <- vertices;
            numberOfStitches <- 1 to scala.math.min(vertexToAdd.perimeter, G.numberOfBoundaryPoints);
            whereToStart <- (numberOfStitches + 1) to G.numberOfBoundaryPoints;
            vertexRotation <- 0 until vertexToAdd.allowedRotationStep
          ) yield {
            Upper(whereToStart, vertexToAdd, vertexRotation, numberOfStitches, None)
          }
        val elementsThatDoConnectToFirstString =
          for (
            vertexToAdd <- vertices;
            numberOfStitches <- 1 to scala.math.min(vertexToAdd.perimeter, G.numberOfBoundaryPoints);
            whereToStart <- 1 to numberOfStitches;
            vertexRotation <- 0 until vertexToAdd.allowedRotationStep;
            basepointOffset <- 0 until (vertexToAdd.perimeter - numberOfStitches - (if (numberOfStitches == G.numberOfBoundaryPoints) 1 else 0))
          ) yield {
            Upper(whereToStart, vertexToAdd, vertexRotation, numberOfStitches, Some(basepointOffset))
          }
        val results = elementsThatDontConnectToFirstString ++ elementsThatDoConnectToFirstString

        results
      }
      override def act(g: Unit, upper: Upper) = upper
    }
    override lazy val lowerObjects = new automorphisms.ActionOnFiniteSet[Lower] {
      // we must only delete a vertex from each most clockwise position it is visible from! 
      override val elements = {
        val intervalsAndVisibleVertices = for (i <- 0 until scala.math.max(G.numberOfBoundaryPoints, 1); j <- G.allVerticesAdjacentToFace(G.boundaryFaces(i mod G.numberOfBoundaryPoints)); if j != 0) yield {
          (i, j)
        }
        info(intervalsAndVisibleVertices)
        def mostClockwise(n: Int, L: Seq[Int]): Seq[Int] = {
          require(L.distinct == L)
          require(L.forall(i => (0 <= i && i < n)))
          if (L.size == n) {
            List(0)
          } else {
            L.filter(i => !L.contains(i + 1 mod n))
          }
        }
        intervalsAndVisibleVertices
          .groupBy(_._2).mapValues(l => l.map(_._1)) // this produces a map, vertices -> visible intervals
          .mapValues(l => mostClockwise(G.numberOfBoundaryPoints, l)) // a map, vertices -> most clockwise visible intervals
          .flatMap(p => p._2.map(i => Lower(i, p._1)))
      }
      override def act(g: Unit, lower: Lower) = lower
    }

    override val ordering: Ordering[lowerObjects.Orbit] = {
      import Ordering.Implicits._
      Ordering.by({ o: lowerObjects.Orbit =>
        // TODO make this a lazy pair, so Dreadnaut is not invoked unnecessarily.
        // TODO consider changing the 4 below, as an optimisation, possibly to a variable depending on G.
        (
          if (G.deleting_vertex_disconnects_graph_?(o.representative.vertexToRemove)) 1 else 0,
          G.dangliness.map(d => -d(o.representative.vertexToRemove)).take(4),
          Dreadnaut.canonicalLabelling(o.representative.encodeAsPlanarGraph.nautyGraph))
      })
    }

  }

}
