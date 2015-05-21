package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.spiders._
import scala.math._
import breeze.linalg._
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}
import scala.sys.process._
import scala.annotation._

object DrawPlanarGraph {
  def apply(G: PlanarGraph, boundaryWeight: Double = 1.0, maxBend: Double = 45, imageScale: Double = 1.5): String = {
    // Outputs LaTeX TikZ code
    // boundaryWeight controls how much to weight the boundary points when calculating the positions of the internal vertices.
    // Weight > 1 (~1.4) is generally good for single component graphs with many internal edges, but may output poor results
    // for graphs with multiple components.
    // maxBendAngle is the largest angle through which to bend multiple edges
    // Doesn't handle self-edges, doesn't draw free loops
    
    assert(G.vertexFlags(0).nonEmpty, "We don't yet handle non-boundary connected graphs")
    if (G.loops > 0) println(s"Warning: graph has ${G.loops} loops which are not shown")
    
    def round(x: Double, n: Int): Double = rint(x * pow(10, n)) / pow(10, n) // Rounds x to n decimal places, used to round coordinate values
    def boundaryPoint(i: Int) = G.numberOfVertices + i // Boundary points are listed after the zeroth and the internal vertices.
                                                       // So if the internal vertices are 1, ..., k, then
                                                       // the 0th, ..., (n-1)th boundary points (in clockwise order around the 0 vertex)
                                                       // are labeled k+1, ..., k+n, in anticlockwise order around the disk boundary.
    def sortTuple(t: Tuple2[Int, Int]) = if (t._1 <= t._2) t else (t._2, t._1)

    // Compute edges as (vertex, vertex) pairs, and vertex adjacencies of the internal vertices.
    var edgesToDraw = Seq[(Int, Int)]()
    val vertexAdjs = (for (v <- G.vertices) yield G.neighboursOf(v)).map(_.toArray) // Vertices adjacent to each vertex, CW order. Includes zero, doesn't include boundary points.

    val vertexFlagsWithRightwardFaces = for (i <- 0 until G.numberOfBoundaryPoints)
      yield (G.vertexFlags.head(i)._1, G.vertexFlags.head( (i+1) % G.vertexFlags.head.length )._2) // Used to find loops at zero
    
    // Get boundary-connected edges.
    for (k <- 0 until G.numberOfBoundaryPoints) {
      val edge = G.vertexFlags.head(k)._1
      val target = G.target(0, edge)
      if (target != 0) { // If target is an internal vertex
        edgesToDraw = (target, boundaryPoint(k)) +: edgesToDraw // Add edge
        // And update adjacency to the correct boundary point
        val index = G.vertexFlags(target).indexOf(G.vertexFlags(target).find(_._1 == edge).get)
        assert(index != -1, "Problem parsing boundary edges")
        vertexAdjs(target)(index) = boundaryPoint(k)
      } else { // Else we have a loop at zero, i.e. an edge with both endpoints on the boundary
        val targetBoundaryPointIndex = vertexFlagsWithRightwardFaces.indexOf(G.vertexFlags.head(k), k)
        if (targetBoundaryPointIndex != -1) {
          edgesToDraw = (boundaryPoint(k), boundaryPoint(targetBoundaryPointIndex)) +: edgesToDraw
        }
      }
    }
    // Append internal edges and ensure each tuple is sorted
    edgesToDraw = (edgesToDraw ++ (for (e <- G.internalEdges) yield G.edgeVertexIncidences(e))).map(sortTuple)
    
    // Now handle drawing coordinates
    // Fix coords of boundary points and calculate coords of internal vertices as the weighted barycenters of their neighbours.
    val boundaryPointCoords = for (k <- 0 until G.numberOfBoundaryPoints)
                              yield (round(cos(Pi + 2*Pi*k / G.numberOfBoundaryPoints), 6),
                                     round(sin(Pi + 2*Pi*k / G.numberOfBoundaryPoints), 6))
    val internalVertexCoords =
      if (G.numberOfInternalVertices != 0) {
        val M = DenseMatrix.zeros[Double](G.numberOfInternalVertices, G.numberOfInternalVertices + G.numberOfBoundaryPoints)
        for (v <- 1 until vertexAdjs.length) {
          M(v - 1, v - 1) = -vertexAdjs(v).distinct.length // M(v-1,v-1) = -#neighbours of vertex v (we don't need the coords of the zeroth vertex) 
          for (w <- vertexAdjs(v)) { // M(v,w) = 1 for all neighbours w of v, modulo appropriate indexing
            M(v - 1, w - 1) = if (w > G.numberOfInternalVertices) boundaryWeight else 1.0 // Weight boundary points more heavily to pull graph closer to boundary 
          }
        }
        val bxs = DenseVector(boundaryPointCoords.map(_._1).toArray)
        val bys = DenseVector(boundaryPointCoords.map(_._2).toArray)
        val A = M(::, 0 until G.numberOfInternalVertices)
        val B = M(::, G.numberOfInternalVertices until G.numberOfInternalVertices + G.numberOfBoundaryPoints)
        val intxs = A \ -(B * bxs)
        val intys = A \ -(B * bys)
        for (v <- 0 until G.numberOfInternalVertices) yield (round(intxs(v), 6), round(intys(v), 6))
      } else IndexedSeq.empty
    
    // Write the TikZ
    var tikzString = s"""\\begin{tikzpicture}
                        |[scale=$imageScale, every node/.style={draw, circle, fill=white, inner sep=0pt, outer sep=0pt, minimum size=2.5pt}]
                        |\\draw[gray, dashed] (0,0) circle (1.0);\n""".stripMargin
    // Place boundary points
    for (i <- 0 until G.numberOfBoundaryPoints) {
      tikzString = tikzString ++ s"\\node (${boundaryPoint(i)}) at ${boundaryPointCoords(i)} {};\n"
    }
    // Place internal vertices
    for (i <- 0 until G.numberOfInternalVertices) {
      tikzString = tikzString ++ s"\\node (${i+1}) at ${internalVertexCoords(i)} {};\n"
    }
    
    // Draw edges between vertices.
    // Need to bend multi-edges and boundary edges appropriately. For now we set a fixed maximum bend angle passed as a parameter,
    // might need to look at this code again if edges start to cross.
    def drawEdgeTikz(endpoints: (Int, Int), multiplicity: Int, maxBendAngle: Double): String = {
      val d = 2*maxBendAngle/(multiplicity-1)
      val angles = if ( multiplicity % 2 == 0 ) (for (i <- 0 to multiplicity/2 - 1) yield d/2 + i*d).flatMap( (x) => Seq(x,-x) )
                   else 0.0 +: (for (i <- 1 to (multiplicity-1)/2) yield i * d).flatMap( (x) => Seq(x,-x) )
      return angles.map( (x) => s"\\draw (${endpoints._1}) to [bend left=${x}] (${endpoints._2});\n").mkString
    }
    
    def bend(u: Int, v: Int): Double =
      // Messy; calculate bend angle for edges u -> v with both u and v on the boundary.
      // Linearly interpolate between bend angle of 45 degrees for adjacent boundary points and 20 degrees for points that are one shy of forming a diagonal
      if (v - u == G.numberOfBoundaryPoints/2) 0
      else if (G.numberOfBoundaryPoints == 4) {
        if (u == G.numberOfInternalVertices + 1 && v > G.numberOfInternalVertices + G.numberOfBoundaryPoints/2) -45
        else 45
      }
      else if (v - u < G.numberOfBoundaryPoints/2) 45 + 50*(v - u - 1)/(4-G.numberOfBoundaryPoints)
      else -bend( u, 2*(u + G.numberOfBoundaryPoints/2)-v )
    
    val edgesWithMultiplicities = edgesToDraw.map( (x) => (x, edgesToDraw.count((y)=>y==x)) ).distinct
    
    for ((e,m) <- edgesWithMultiplicities) {
      if (e._1 > G.numberOfInternalVertices) { // Edge with both endpoints on boundary
        tikzString = tikzString ++ s"\\draw (${e._1}) to [bend left=${bend(e._1, e._2)}] (${e._2});\n"
      }
      else { // All other edges
        tikzString = tikzString ++ drawEdgeTikz(e, m, maxBend)
      }
    }
    
    tikzString = tikzString ++ "\\end{tikzpicture}"
    return tikzString
  }
  
  def pdf(Gs: Seq[PlanarGraph], pdfPath: String, boundaryWeight: Double = 1.0, maxBend: Double = 45, imageScale: Double = 1.5): Unit = {
    // Writes TikZ to tex file and runs pdflatex
    val outputStr = Gs.map((x)=>DrawPlanarGraph(x, boundaryWeight, maxBend, imageScale)).mkString(
        "\\documentclass{article}\n\\usepackage{tikz}\n\\begin{document}\n",
        "\\bigskip\\bigskip\n\n",
        "\n\\end{document}")
    val dir = pdfPath.reverse.dropWhile(_!='/').reverse
    val texPath = pdfPath.stripSuffix(".pdf") ++ ".tex"
    Files.write(Paths.get(texPath),(outputStr).getBytes(StandardCharsets.UTF_8))
    try s"pdflatex -output-directory=$dir $texPath".!!
    catch {
      case e:java.lang.Exception => { println("Error: Problem running pdflatex! Maybe check the filename and that the path exists?"); throw e }
    }
  }
}