package net.tqft.toolkit.algebra.spiders

import scala.language.implicitConversions
import net.tqft.toolkit.algebra.enumeration.Odometer
import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration
import net.tqft.toolkit.algebra.grouptheory.FiniteGroup
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.arithmetic.Mod._

case class GraphsGeneratedBy2(vertexTypes: Seq[VertexType]) {
  private def spider = implicitly[DiagramSpider[PlanarGraph]]

  case class avoiding(faces: Seq[PlanarGraph]) {

    def byNumberOfFaces(numberOfBoundaryPoints: Int, numberOfFaces: Int): Stream[PlanarGraph] = {

      case class Node(p: PlanarGraph) extends CanonicalGeneration[Node, Int] {
        override lazy val automorphisms: FinitelyGeneratedFiniteGroup[Int] = ???
        
        sealed trait Upper {
          val result: Node = ???
          def inverse: result.Lower
        }
        case class RemoveBoundaryArc(position: Int) extends Upper {
          override lazy val inverse = result.InsertArc(position)
        }
        case class RemoveBoundaryVertex(position: Int) extends Upper {
          override lazy val inverse = result.AttachVertex(???, ???, ???, ???)
        }
        
        override lazy val upperObjects: automorphisms.Action[Upper] = new automorphisms.Action[Upper] { 
          def elements: Seq[Upper] = ???
          def act(g: Int, u: Upper): Upper = u match {
            case RemoveBoundaryArc(position) => RemoveBoundaryArc((position + g) mod p.numberOfBoundaryPoints)
          }
        }
        
        sealed trait Lower {
          def result: Node
        }
        case class InsertArc(position: Int) extends Lower {
          override def result: Node = ???
        }
        case class AttachVertex(size: Int, rotation: Int, connectivity: Int, position: Int) extends Lower {
          override def result: Node = ???
        }
        
        override lazy val lowerObjects: automorphisms.Action[Lower] = new automorphisms.Action[Lower] { 
          def elements: Seq[Lower] = ???
          def act(g: Int, u: Lower): Lower = u match {
            case InsertArc(position) => InsertArc((position + g) mod p.numberOfBoundaryPoints)
            case a: AttachVertex => a.copy(position = (a.position + g) mod p.numberOfBoundaryPoints)
          }
        }
        override lazy val ordering = ???
        
      }

      Node(PlanarGraph.empty)
        .descendants()
        .map(_.p)
        .filter(p => p.numberOfBoundaryPoints == numberOfBoundaryPoints && p.numberOfInternalFaces == numberOfFaces)
        .toStream

    }

  }
}