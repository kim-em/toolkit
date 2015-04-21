package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.spiders._
import scala.math._
import breeze.linalg._

object DrawPlanarGraph {
  def apply(G: PlanarGraph, radius: Double = 2.0, vertexLabels: Boolean = false, imageScale: Double = 1): String = {
    // Use Tutte's barycenter method to draw planar graphs.
    // Outputs LaTeX tikz code.
    
    // A bit messy: we need to ignore the 0th vertex of the graph, this throws all our indices off by one.
    // Helper functions:
    def indexOfVertex(v: Int) = v - 1
    def vertexAtIndex(i: Int) = i + 1
    def round(x: Double, n: Int): Double = rint(x * pow(10, n)) / pow(10, n) // Rounds x to n decimal places, used to round coordinate values

    val numOfVertices = G.numberOfVertices - 1 // Remember to ignore 0 !!
    val peripheralVertices = G.neighboursOf(0).distinct // Vertices of outermost cycle; these are the ones adjacent to the boundary points
    val nonperipheralVertices = G.vertices diff peripheralVertices diff List(0) // All internal vertices
    //Debug
    //println("Peripheral vertices: " + peripheralVertices)
    //println("Nonperipheral vertices: " + nonperipheralVertices)
    
    val vertexCoords = Array.fill(numOfVertices)((0.0, 0.0))
    
    val bdryPts = Array.fill(peripheralVertices.length)( Seq[(Double, Double)]( )) // ith sequence corresponds to the bdry pts adjacent to ith entry of peripheralVertices
    
    // Handle boundary points & outermost vertices of planar graph
    for (k <- 0 until peripheralVertices.length) {
      // First place peripheral vertices
      val theta = 2 * k * Pi / peripheralVertices.length
      val x = round(radius * cos(theta), 6)
      val y = round(radius * sin(theta), 6)
      vertexCoords(indexOfVertex(peripheralVertices(k))) = (x, y)
      
      // Then place boundary points adjacent to each peripheral vertex
      val n = G.neighboursOf(peripheralVertices(k)).count(_ == 0)
      bdryPts(k) = ( for ( k <- 1 to n )
                             yield ( round(0.2 * radius * cos(theta + Pi/2 - k*Pi/(n+1)), 6) + x ,
                                     round(0.2 * radius * sin(theta + Pi/2 - k*Pi/(n+1)), 6) + y)
                          ).toSeq
    }
    //vertexCoords.map(println(_)) //Debug

    // If there are vertices remaining, place them by barycentric mapping
    if (!nonperipheralVertices.isEmpty) {
      val A = DenseMatrix.zeros[Double](numOfVertices, numOfVertices) -
              diag( DenseVector.tabulate(numOfVertices) {
                i => ( G.neighboursOf(vertexAtIndex(i)).distinct diff Seq(0) ).length.toDouble
              }) // 0 doesn't exist in the graph we're drawing, so don't count any edge adjacent to it
      for (i <- 0 until numOfVertices) {
        val neighbours = G.neighboursOf(vertexAtIndex(i)).distinct diff List(0)
        for (n <- neighbours.distinct) { A(i, indexOfVertex(n)) = 1.0 }
      }
      //println("A:\n" + A) //Debug
      
      // Solve linear system for coords of nonperipheral vertices 
      val B = A(::, nonperipheralVertices.toList.map(indexOfVertex(_))).toDenseMatrix
      val x = -A * DenseVector(vertexCoords.map(_._1))
      val y = -A * DenseVector(vertexCoords.map(_._2))
      val nonperipheralXs = B \ x
      val nonperipheralYs = B \ y
      //println(B); println(nonperipheralXs); println(nonperipheralYs)
      
      // Update vertexCoords
      for (i <- 0 until nonperipheralVertices.length) {
        vertexCoords(indexOfVertex(nonperipheralVertices(i))) = (round(nonperipheralXs(i), 6), round(nonperipheralYs(i), 6))
      }
      //vertexCoords.map(println(_))
    }
    
    // Get graph edges. For now, just one per connected pair of vertices. Will implement multiple edges soon.
    var edgePairs = Seq[(Int,Int)]()
    for (pair <- G.edgeVertexIncidences.values) {
      if (pair._1 != 0 && pair._2 != 0) edgePairs = pair +: edgePairs
    }
    
    // Write the tikz!
    var tikzString = s"""\\begin{tikzpicture}
                        |[scale=$imageScale, every node/.style={circle, fill=white, inner sep=0pt, outer sep=0pt, minimum size=1pt}]""".stripMargin
    // Draw boundary points
    for (i <- 0 until peripheralVertices.length) for (j <- 0 until bdryPts(i).length) {
      tikzString = tikzString ++ s"\n\\node (${peripheralVertices(i)}-$j) at ${bdryPts(i)(j)} {};"
    }
    // Draw graph vertices
    for (i <- 0 until vertexCoords.length) {
      tikzString = tikzString ++ s"\n\\node (${vertexAtIndex(i)}) at ${vertexCoords(i)} {${if (vertexLabels) vertexAtIndex(i) else ""}};"
    }
    // Draw internal edges:
    val edgePairsString = edgePairs map ( (p:(Int,Int)) => "%d/%d".format(p._1,p._2) ) mkString ","
    // Draw boundary edges:
    val bdryEdgesString = (for (i <- 0 until peripheralVertices.length; j <- 0 until bdryPts(i).length)
                             yield s"${peripheralVertices(i)}/${peripheralVertices(i)}-$j"
                           ) mkString ","
    // Wrap up:
    tikzString = tikzString ++ s"""\n\\foreach \\from/\\to in {$edgePairsString,$bdryEdgesString}
                                  |\\draw (\\from) -- (\\to);
                                  |\\end{tikzpicture}""".stripMargin

    return tikzString
  }
}