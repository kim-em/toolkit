package net.tqft.toolkit.algebra.spiders

import scala.language.implicitConversions
import net.tqft.toolkit.algebra.enumeration.Odometer

case class VertexType(perimeter: Int, allowedRotationStep: Int)

object VertexType {
  implicit def lift(p: (Int, Int)) = VertexType(p._1, p._2)
}

case class GraphsGeneratedBy(vertexTypes: Seq[VertexType]) {
  private def spider = implicitly[DiagramSpider[PlanarGraph]]

  private def largestPerimeter = (0 +: vertexTypes.map(_.perimeter)).max

  case class avoiding(faces: Seq[PlanarGraph]) {

    def byNumberOfFaces(numberOfBoundaryPoints: Int, numberOfFaces: Int): Stream[PlanarGraph] = {
      // let f denote the number of internal faces, n the number of boundary points
      // then 2 f <= \sum_v (degree(v) - 2) <= n - 2 + 2f
      val limit = { k: List[Int] => k.zip(vertexTypes.map(_.perimeter - 2)).map(p => p._1 * p._2).sum <= numberOfBoundaryPoints - 2 + 2 * numberOfFaces}
      Odometer(limit)(List.fill(vertexTypes.size)(0)).toStream.flatMap(k => byNumberOfVertices(numberOfBoundaryPoints, vertexTypes.zip(k).toMap))
    }

    private var stackDepth = 0

    private def byNumberOfVertices_(numberOfBoundaryPoints: Int, numberOfVertices: Map[VertexType, Int]): Seq[PlanarGraph] = {
      stackDepth = stackDepth + 1
      //      print("\n" + List.fill(stackDepth)(' ').mkString + " " + numberOfBoundaryPoints + " " + numberOfVertices)

      if (numberOfBoundaryPoints < 0 || numberOfVertices.values.exists(_ < 0)) {
        //        print(".")
        stackDepth = stackDepth - 1
        Stream.empty
      } else if (numberOfBoundaryPoints == 0 && numberOfVertices.values.forall(_ == 0)) {
        //        print(".")
        stackDepth = stackDepth - 1
        Stream(PlanarGraph.empty)
      } else {
        /*
         *  We first describe how to take apart a PlanarGraph.
         *  
         *  If there is a strand at the boundary, remove that.
         *  Otherwise, remove the most dangly vertex.
         *   
         *  Since we always prefer to remove a strand, there's no need to add vertices unless afterwards there are no boundary strands.
         */

        def numberOfBoundaryStrands(p: PlanarGraph) = {
          p.edgesAdjacentTo(0).map(edge => p.target(0, edge)).count(_ == 0) / 2
        }
        def numberOfBoundaryVertices(p: PlanarGraph) = {
          p.vertices.tail.count(v => p.neighboursOf(v).forall(_ == 0))
        }

        def addVertex(v: VertexType, connectivity: Int)(graph: PlanarGraph): Seq[PlanarGraph] = {
          for (
            j <- 0 until graph.numberOfBoundaryPoints + v.perimeter;
            k <- 0 until v.allowedRotationStep;
            c = spider.rotate(
              spider.multiply(
                spider.rotate(graph, Seq(graph.numberOfBoundaryPoints - 1, j).max),
                spider.rotate(PlanarGraph.star(v.perimeter, v.allowedRotationStep), k),
                connectivity),
              -j);
            if c.dangle == v.perimeter - connectivity
          ) yield {
            c
          }
        }
        def addCups(graph: PlanarGraph) = for (j <- 0 until graph.numberOfBoundaryPoints + 2) yield {
          spider.rotate(spider.tensor(spider.rotate(graph, Seq(graph.numberOfBoundaryPoints - 1, j).max), PlanarGraph.strand), -j)
        }

        val candidates = {
          val addCupCandidates = {
            for (
              p <- byNumberOfVerticesCache(numberOfBoundaryPoints - 2, numberOfVertices).iterator;
              c <- addCups(p).iterator
            ) yield {
              c
            }
          }

          val addVertexCandidates = {
            for (
              v <- numberOfVertices.keys;
              if numberOfVertices(v) > 0;
              newNumberOfVertices = numberOfVertices.updated(v, numberOfVertices(v) - 1);
              connectivity <- 0 to (if (numberOfBoundaryPoints == 0) v.perimeter else v.perimeter - 1);
              // TODO this line seems to be wrong --- verify and delete?
              //              if newNumberOfVertices.map(p => p._1.perimeter * p._2).sum >= connectivity;
              p <- byNumberOfVerticesCache(numberOfBoundaryPoints - v.perimeter + 2 * connectivity, newNumberOfVertices).iterator;
              c <- addVertex(v, connectivity)(p).iterator;
              if numberOfBoundaryStrands(c) == 0
            ) yield {
              c
            }
          }

          addCupCandidates ++ addVertexCandidates
        }

        val distinct = Set() ++ candidates.map(spider.canonicalFormWithDefect).map(_._1)

        val result = distinct.filter(g => g.loops == 0 && faces.forall(f => g.Subgraphs(f).excisions.isEmpty))
        //        print(".")
        stackDepth = stackDepth - 1
        result.toSeq.sortBy(_.numberOfInternalFaces)

      }

    }

    private val byNumberOfVerticesCache = {

      //      val dbFile = new java.io.File("graphs-generated-by-db")
      //      import org.mapdb._
      //      val db = DBMaker.newFileDB(dbFile)
      //        .closeOnJvmShutdown
      //        .make
      //
      //      import scala.collection.JavaConverters._
      //      val store = db.getHashMap[(Seq[VertexType], Seq[PlanarGraph], Int, Map[VertexType, Int]), Seq[PlanarGraph]]("terms").asScala
      //
      //      { (n: Int, k: Map[VertexType, Int]) =>
      //        this.synchronized {
      //          var miss = false
      //          val result = store.getOrElseUpdate((vertexTypes, faces, n, k),
      //            {
      //              println(s"cache miss: $n, $k")
      //              miss = true
      //              byNumberOfVertices_(n, k)
      //            })
      //          if (miss) db.commit
      //          result
      //        }
      //      }

      import net.tqft.toolkit.functions.Memo._
      val cached = ({ t: (Int, Map[VertexType, Int]) => byNumberOfVertices_(t._1, t._2) }).memo;
      { (n: Int, k: Map[VertexType, Int]) => cached((n, k)) }
    }

    def byNumberOfVertices(numberOfBoundaryPoints: Int, numberOfVertices: Map[VertexType, Int]): Seq[PlanarGraph] = byNumberOfVerticesCache(numberOfBoundaryPoints, numberOfVertices)
    def byNumberOfVertices(numberOfBoundaryPoints: Int, numberOfVertices: Int): Seq[PlanarGraph] = {
      require(vertexTypes.size == 1)
      byNumberOfVertices(numberOfBoundaryPoints, Map(vertexTypes.head -> numberOfVertices))
    }
  }
}