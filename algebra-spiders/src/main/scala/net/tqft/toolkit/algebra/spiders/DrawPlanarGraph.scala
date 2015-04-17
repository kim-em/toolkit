package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.spiders._
import scala.math._
import breeze.linalg._

object DrawPlanarGraph {
  def apply(G: PlanarGraph, radius: Double = 2.0): String = {
    // Use Tutte's barycenter method to draw planar graphs.
    // Outputs LaTeX tikz code.
    
    // A bit messy: we need to ignore the 0th vertex of the graph, this throws all our indices off by one.
    // Helper functions:
    def indexOfVertex(v: Int) = v - 1
    def vertexAtIndex(i: Int) = i + 1
    def round(x: Double, n: Int): Double = rint(x * pow(10, n)) / pow(10, n) // Rounds x to n decimal places, used to round coordinate values

    val numOfVertices = G.numberOfVertices - 1 // Remember to ignore 0 !!
    val vertexCoords = Array.fill(numOfVertices)((0.0, 0.0))

    val peripheralVertices = G.neighboursOf(0).distinct // Vertices of outermost cycle; these are the ones adjacent to the boundary points
    val nonperipheralVertices = G.vertices diff peripheralVertices diff List(0) // All internal vertices
    //Debug
    //println("Peripheral vertices: " + peripheralVertices)
    //println("Nonperipheral vertices: " + nonperipheralVertices)

    // First place peripheral vertices
    for (k <- 0 until peripheralVertices.length) {
      vertexCoords(indexOfVertex(peripheralVertices(k))) = (round(radius * cos(2 * k * Pi / peripheralVertices.length), 6), round(radius * sin(2 * k * Pi / peripheralVertices.length), 6))
    }
    //vertexCoords.map(println(_)) //Debug

    // If there are vertices remaining, place them by barycentric mapping
    if (!nonperipheralVertices.isEmpty) {
      val A = DenseMatrix.zeros[Double](numOfVertices, numOfVertices) -
              diag( DenseVector.tabulate(numOfVertices) {
                i => ( G.neighboursOf(vertexAtIndex(i)).distinct.length +
                       (if (G.neighboursOf(0) contains vertexAtIndex(i)) -1 else 0)
                     ).toDouble
              })
                     // ^ 0 doesn't exist in the graph we're drawing, so don't count any edge adjacent to it
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
    
    // Write the tikz!
    var tikzString = """\begin{tikzpicture}
                       |[scale=1, every node/.style={circle, fill=black, inner sep=0pt, outer sep=0pt, minimum size=0pt}]""".stripMargin
    // Place nodes
    for (i <- 0 until vertexCoords.length) {
      tikzString = tikzString ++ s"\n\\node (${vertexAtIndex(i)}) at ${vertexCoords(i)} {};"
    }
    // Draw edges. For now, just one per connected pair of vertices. Will implement multiple edges soon.
    var edgePairs = Seq[(Int,Int)]()
    for (pair <- G.edgeVertexIncidences.values) {
      if (pair._1 != 0 && pair._2 != 0) edgePairs = pair +: edgePairs
    }
    val edgePairsString = edgePairs map ( (p:(Int,Int)) => "%d/%d".format(p._1,p._2) ) mkString ","
    tikzString = tikzString ++ s"""\\foreach \\from/\\to in {${edgePairsString}}
                                  |\\draw (\\from) -- (\\to);
                                  |\\end{tikzpicture}""".stripMargin

    return tikzString
  }
}