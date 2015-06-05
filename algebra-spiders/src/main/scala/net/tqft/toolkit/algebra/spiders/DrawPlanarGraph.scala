package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.spiders._
import scala.math._
import breeze.linalg._
import java.nio.charset.StandardCharsets
import java.nio.file.{ Paths, Files }
import scala.sys.process._
import scala.annotation._
import scala.util.Try

trait DrawPlanarGraph {
  // boundaryWeight controls how much to weight the boundary points when calculating the positions of the internal vertices.
  // Weight > 1 (~1.4) is generally good for single component graphs with many internal edges, but may output poor results
  // for graphs with multiple components.
  def boundaryWeight: Double
  def imageScale: Double

  def withBoundaryWeight(boundaryWeight: Double) = CustomizedDrawPlanarGraph(boundaryWeight, imageScale)
  def withImageScale(imageScale: Double) = CustomizedDrawPlanarGraph(boundaryWeight, imageScale)

  var pdflatexPath: String = {
    val paths = for (
      dir <- List("/usr/texbin");
      file = new java.io.File(dir + "/pdflatex");
      if file.exists
    ) yield file.toPath.toString

    paths.headOption.orElse(Try("which pdflatex".!!).toOption).getOrElse("pdflatex")
  }

  def apply(G: PlanarGraph): String = {
    // Outputs LaTeX TikZ code
    // Doesn't draw free loops

    assert(G.vertexFlags(0).nonEmpty, "We don't yet handle non-boundary connected graphs")
    if (G.loops > 0) println(s"Warning: graph has ${G.loops} loops which are not shown")

    def round(x: Double, n: Int): Double = rint(x * pow(10, n)) / pow(10, n) // Rounds x to n decimal places, used to round coordinate values
    def boundaryPoint(i: Int) = G.numberOfVertices + i // Boundary points are listed after the zeroth and the internal vertices.
      // So if the internal vertices are 1, ..., k, then
      // the i = 0, ..., n-1 boundary points (in clockwise order around the 0 vertex)
      // are labeled k+1, ..., k+n, in anticlockwise order around the disk boundary.
    def sortTuple(t: Tuple2[Int, Int]) = if (t._1 <= t._2) t else (t._2, t._1)
    def cyclicReverse[A](xs: Seq[A]) = xs.head +: xs.tail.reverse

    // SET UP GRAPH MODEL
    var edgeEndpts = Seq[(Int, (Int, Int))]() // All edges in graph, in the format (Edgelabel, (Endpoint1, Endpoint2))
    val vertexAdjs = (for (v <- G.vertices) yield G.neighboursOf(v)).map(_.toArray) // Vertices adjacent to each vertex, CW order. Includes zero, doesn't include boundary points.
      // Used to compute coordinates of vertices to draw.
    val vertexFlagsWithRightwardFaces = for (i <- 0 until G.numberOfBoundaryPoints)
      yield (G.vertexFlags.head(i)._1, G.vertexFlags.head((i + 1) % G.vertexFlags.head.length)._2) // Used to find loops at zero

    // Get boundary-connected edges.
    for (k <- 0 until G.numberOfBoundaryPoints) {
      val edge = G.vertexFlags.head(k)._1
      val target = G.target(0, edge)
      if (target != 0) { // If target is an internal vertex
        edgeEndpts = (edge, (target, boundaryPoint(k))) +: edgeEndpts // Add edge, ensuring that the boundary vertex is the second entry
        // And update adjacency to the correct boundary point
        val index = G.vertexFlags(target).indexOf(G.vertexFlags(target).find(_._1 == edge).get)
        assert(index != -1, "Problem parsing boundary edges")
        vertexAdjs(target)(index) = boundaryPoint(k)
      } else { // Else we have a loop at zero, i.e. an edge with both endpoints on the boundary
        val targetBoundaryPointIndex = vertexFlagsWithRightwardFaces.indexOf(G.vertexFlags.head(k), k)
        if (targetBoundaryPointIndex != -1) {
          edgeEndpts = (edge, (boundaryPoint(k), boundaryPoint(targetBoundaryPointIndex))) +: edgeEndpts
        }
      }
    }
    // Append all other edges
    edgeEndpts = edgeEndpts ++ (for (edge <- G.internalEdges) yield (edge, G.edgeVertexIncidences(edge)))
    
    // NODES
    // Fix coords of boundary points and calculate coords of internal vertices as the weighted barycenters of their neighbours.
    val boundaryPointCoords = for (k <- 0 until G.numberOfBoundaryPoints)
      yield (round(cos(Pi + 2 * Pi * k / G.numberOfBoundaryPoints), 6),
      round(sin(Pi + 2 * Pi * k / G.numberOfBoundaryPoints), 6))
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
    
    // Calculate vertex rotation that minimizes edge deviation
    def minimizingRotation(vertexCoords: Tuple2[Double, Double], neighbourCoords: Seq[Tuple2[Double, Double]]): Double = {
      val numberOfNeighbours = neighbourCoords.length
      val edgeOutAngles = for (i <- 0 until numberOfNeighbours) yield i * 2 * Pi / numberOfNeighbours
      val neighbourAngles = for ((x,y) <- neighbourCoords) yield {
        if (vertexCoords == (x,y)) 0
        else {val angle = atan2(y - vertexCoords._2, x - vertexCoords._1); if (angle < 0) angle + 2 * Pi else angle}
      }
      (0 until 360).minBy((x) => 
        (for (i <- 0 until edgeOutAngles.length if neighbourCoords(i) != vertexCoords) yield pow(sin(abs(edgeOutAngles(i) + x * Pi / 180 - neighbourAngles(i)) / 2), 2)).sum) 
    }
    val vertexRotations: Seq[Double] =
      (for (i <- 0 until G.numberOfInternalVertices) yield {
        val CWNeighbours = for (v <- vertexAdjs(i + 1))
          yield if (v > G.numberOfInternalVertices) boundaryPointCoords(v - G.numberOfInternalVertices - 1) else internalVertexCoords(v - 1)
        minimizingRotation(internalVertexCoords(i), cyclicReverse(CWNeighbours)) // Traverse edges in ACW order to account for the ACW rotation we calculated earlier
      }) ++ boundaryPointCoords.map((t: (Double, Double)) => atan2(-t._2, -t._1) * 180 / Pi).map(round(_, 6))
    
    // WRITE TikZ
    var tikzString = s"""\\begin{tikzpicture}
                        |[scale=$imageScale, every node/.style={draw, circle, fill=white, inner sep=0pt, outer sep=0pt, minimum size=2.5pt}]
                        |\\draw[gray, dashed] (0,0) circle (1.0);\n""".stripMargin
    // Place boundary points
    for (i <- 0 until G.numberOfBoundaryPoints) {
      tikzString = tikzString ++ s"\\node (${boundaryPoint(i)}) at ${boundaryPointCoords(i)} {};\n"
    }
    // Place internal vertices
    for (i <- 0 until G.numberOfInternalVertices) {
      tikzString = tikzString ++ s"\\node (${i + 1}) at ${internalVertexCoords(i)} {};\n"
    }
    // Draw edges
    def getAngle(edge: Int, endpoint: Int): Double =
      if (endpoint > G.numberOfInternalVertices) // If endpoint is a boundary vertex
        vertexRotations(endpoint - 1)
      else {
        assert(G.edgesAdjacentTo(endpoint).indexOf(edge) != -1)
        val ACWEdgeAdjs = cyclicReverse(G.edgesAdjacentTo(endpoint))
        ACWEdgeAdjs.indexOf(edge) * 360 / G.degree(endpoint) + vertexRotations(endpoint - 1)
      }
    for (t: Tuple2[Int, (Int,Int)] <- edgeEndpts) {
      val edge = t._1
      val u = t._2._1
      val v = t._2._2
      val edgeStr = if (u == v) { // Self-loop
        val firstEdgeIndex = cyclicReverse(G.edgesAdjacentTo(u)).indexOf(edge)
        val secondEdgeIndex = cyclicReverse(G.edgesAdjacentTo(u)).indexOf(edge, firstEdgeIndex + 1)
        assert(firstEdgeIndex != -1 && secondEdgeIndex != -1)
        val outAngle = firstEdgeIndex * 360 / G.degree(u) + vertexRotations(u - 1)
        val inAngle = secondEdgeIndex * 360 / G.degree(u) + vertexRotations(u - 1)
        s"\\draw ($u) to [out=$outAngle, in=$inAngle, loop] ($u);\n"
      } else { // Ordinary edge
        s"\\draw ($u) to [out=${getAngle(edge, u)}, in=${getAngle(edge, v)}] ($v);\n"
      }
      tikzString = tikzString ++ edgeStr
    }

    tikzString = tikzString ++ "\\end{tikzpicture}"
    return tikzString
  }

  def pdf(g: PlanarGraph, pdfPath: String) {
    pdf(Seq(g), pdfPath)
  }
  def pdf(Gs: Seq[PlanarGraph], pdfPath: String): Unit = {
    // Writes TikZ to tex file and runs pdflatex
    val outputStr = Gs.map((x) => DrawPlanarGraph(x)).mkString(
      "\\documentclass{article}\n\\usepackage{tikz}\n\\begin{document}\n",
      "\\bigskip\\bigskip\n\n",
      "\n\\end{document}")
    // TODO this is very unsafe; between to use methods provided by Path
    // also, what happens if we just give a relative path --- should pdfPath = "hexagon.pdf" be allowed, to write to the current directory?
    val dir = pdfPath.reverse.dropWhile(_ != '/').reverse 
    val texPath = pdfPath.stripSuffix(".pdf") ++ ".tex"
    Files.write(Paths.get(texPath), (outputStr).getBytes(StandardCharsets.UTF_8))
    val command = pdflatexPath + s" -output-directory=$dir $texPath"
    
    // TODO cleanup temporary files (.out, .aux, .log)
    // TODO ideally, we should also crop to the bounding box, because I'll want to embed these pdfs elsewhere.
    // here's a snippet of mathematica code that shows how, using pdfcrop:
    /*
 gs = FileNames[{"/usr/bin/gs", "/usr/local/bin/gs"}][[1]];
 Run[
 "
   cat top-template > document.tex;
   cat snippet >> document.tex;
   cat bottom-template >> document.tex;
   /usr/texbin/pdflatex document.tex;
   /usr/texbin/pdfcrop document.pdf --gscmd " <> gs <> 
   " --pdftexcmd /usr/texbin/pdftex --verbose --debug;
   " <> gs <> 
   " -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=finished.pdf \
document-crop.pdf;
   rm tmp-pdfcrop-*;
   rm document.*;
   rm snippet;
 "]
     */
    try command.!!
    catch {
      case e: java.lang.Exception => { println(s"Error: Problem running '$command'! Maybe check the filename and that the path exists?"); throw e }
    }
  }
}

object DrawPlanarGraph extends DrawPlanarGraph {
  override val boundaryWeight: Double = 1.0
  override val imageScale: Double = 1.5
}

case class CustomizedDrawPlanarGraph(val boundaryWeight: Double, val imageScale: Double) extends DrawPlanarGraph