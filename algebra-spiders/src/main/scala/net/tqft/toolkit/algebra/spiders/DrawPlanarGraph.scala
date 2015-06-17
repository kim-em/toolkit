package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.spiders._
import scala.math._
import breeze.linalg._
import java.nio.charset.StandardCharsets
import java.nio.file.{ Paths, Files }
import scala.sys.process._
import scala.annotation._
import scala.util.Try
import org.apache.commons.io.FilenameUtils

trait DrawPlanarGraph {
  // boundaryWeight controls how much to weight the boundary points when calculating the positions of the internal vertices.
  // Weight > 1 (~1.4) is generally good for single component graphs with many internal edges, but may output poor results
  // for graphs with multiple components.
  def boundaryWeight: Double
  def imageScale: Double
  def globalStyle: String

  def withBoundaryWeight(boundaryWeight: Double) = CustomizedDrawPlanarGraph(boundaryWeight, imageScale, globalStyle)
  def withImageScale(imageScale: Double) = CustomizedDrawPlanarGraph(boundaryWeight, imageScale, globalStyle)
  def withGlobalStyle(globalStyle: String) = CustomizedDrawPlanarGraph(boundaryWeight, imageScale, globalStyle)

  private def round(x: Double, n: Int): Double = rint(x * pow(10, n)) / pow(10, n) // Rounds x to n decimal places, used to round coordinate values
  private def sortTuple(t: Tuple2[Int, Int]) = if (t._1 <= t._2) t else (t._2, t._1)
  private def cyclicReverse[A](xs: Seq[A]) = xs.head +: xs.tail.reverse

  private def getProgramPath(programName: String, searchDirs: Seq[String]) = {
    val paths = for (
      dir <- searchDirs;
      file = new java.io.File(dir + s"/$programName");
      if file.exists
    ) yield file.toPath.toString

    paths.headOption.orElse(Try(s"which $programName".!!).toOption).getOrElse(programName)
  }
  var pdflatexPath = getProgramPath("pdflatex", List("/usr/texbin"))
  var pdfcropPath = getProgramPath("pdfcrop", List("/usr/texbin"))

  def apply(G: PlanarGraph, crossings: Map[Int, Int] = Map.empty, hideDiskBoundary: Boolean = false): String = {
    // Draws regular, closed and knotted PlanarGraphs by doing some preprocessing and then calling draw.
    // Draws over and undercrossings with or without orientations labeled, according to the parameter crossings: Map[vertex: Int, sign: Int].
    // sign > 0, < 0, = 0 means positive, negative, unoriented crossing resp.

    var modifiedVertexFlags = G.vertexFlags
    var decoratedEdges = Map[Int, String]()
    var decoratedVertices = Map[Int, String]()
    var hideBoundaryEdges = false

    // Process closed graphs with > 1 internal vertices (closed graphs with 1 internal vertex are handled nicely by draw).
    // For each vertex of the outer face we attach an edge to the external vertex, which is hidden when drawn.
    if (G.numberOfBoundaryPoints == 0 && G.numberOfInternalVertices > 1) {
      val outerFaceVertexEdges = G.faceBoundary(G.outerFace).head // Traverse outerFace in CW order, returning sequence of (vertex, edge to next vertex)
      val outerFaceVertices = outerFaceVertexEdges.map(_._1)
      val modifiedInternalVertexFlags = for (vertex <- 1 until G.numberOfVertices) yield {
        if (!(outerFaceVertices contains vertex))
          G.vertexFlags(vertex)
        else {
          val edgeOut = (outerFaceVertexEdges find ((t: (Int, Int)) => t._1 == vertex)).get._2
          val splitFlags = G.vertexFlags(vertex) span ((t: (Int, Int)) => t._1 != edgeOut)
          splitFlags._1 ++ ((-vertex, -vertex) +: splitFlags._2)
        }
      }
      val modifiedExternalVertexFlags = cyclicReverse(outerFaceVertexEdges).map((t: (Int, Int)) => (-t._1, -t._1))
      modifiedVertexFlags = modifiedExternalVertexFlags +: modifiedInternalVertexFlags
      hideBoundaryEdges = true
    }
    
    // Process knotted graphs
    if (crossings.nonEmpty) {
      def sign = crossings
      def start(edge: Int) = G.edgeVertexIncidences(edge)._1
      def end(edge: Int) = G.edgeVertexIncidences(edge)._2
      
      crossings.keys.map((v) => assert(G.degree(v) == 4, s"Vertex $v doesn't appear to be a crossing..."))
      // Decorate vertices as crossings
      decoratedVertices = crossings.mapValues(_ => "minimum size=0pt")
      // Decorate crossing edges
      var undercrossings = Seq[(Int, Int, Int)]() // (edge, vertex, index) means "edge" undercrosses at the endpoint "vertex".
        // "index" is used to distinguish between positive and negative loops.
      var overcrossings = Seq[(Int, Int)]() // (edge, vertex). Index not needed.
      
      for (vertex <- crossings.keys; i <- List(1, 3)) {
        undercrossings = (cyclicReverse(G.edgesAdjacentTo(vertex))(i), vertex, i) +: undercrossings
      }
      for (vertex <- crossings.keys; i <- List(0,2)) {
        overcrossings = (cyclicReverse(G.edgesAdjacentTo(vertex))(i), vertex) +: overcrossings
      } // 0th and 2th edges are overcrossings; 1th and 3th are undercrossings
      
      def notAnUndercrossing(edge: Int): Boolean = {
        val undercrossingEdges = undercrossings.map(_._1)
        !undercrossingEdges.contains(edge)
      }
      overcrossings = overcrossings.filter((t) => notAnUndercrossing(t._1))

      // Process undercrossings: shorten TikZ edges at the correct end and add arrows to indicate orientation if needed
      for ((edge, endpoint, i) <- undercrossings) {
        val styleStr =
          if (start(edge) == end(edge)) { // Edge is a loop
            if (i == 1) {
              if (G.edgesAdjacentTo(endpoint).head == edge) { // Inward undercrossing of negative twist
                assert(sign(endpoint) <= 0, s"The crossing at vertex $endpoint is either negative or nonoriented!")
                "shorten >=2.5pt" + (if (sign(endpoint) < 0) ", ->-" else "")
              }
              else { // Outward undercrossing, positive twist
                assert(sign(endpoint) >= 0, s"The crossing at vertex $endpoint is either positive or nonoriented!")
                "shorten <=2.5pt" + (if (sign(endpoint) > 0) ", ->-" else "")
              }
            } else {
              if (G.edgesAdjacentTo(endpoint).head == edge) { // Inward undercrossing, positive twist
                assert(sign(endpoint) >= 0, s"The crossing at vertex $endpoint is either positive or nonoriented!")
                "shorten >=2.5pt" + (if (sign(endpoint) > 0) ", ->-" else "")
              }
              else { // Outward undercrossing, negative twist
                assert(sign(endpoint) <= 0, s"The crossing at vertex $endpoint is either negative or nonoriented!")
                "shorten >=2.5pt" + (if (sign(endpoint) < 0) ", -<-" else "")
              }
            }
          } else { // Edge is not a loop
            ( // Shorten at the correct end
              if (endpoint == start(edge)) "shorten <=2.5pt"
              else "shorten >=2.5pt"
            ) +
            ( // Add arrows to indicate orientation, if needed
              if (sign(endpoint) == 0) ""
              else if (
                (i == 1 && (sign(endpoint) > 0) && start(edge) == endpoint)
                || (i == 1 && sign(endpoint) < 0 && start(edge) != endpoint)
                || (i == 3 && sign(endpoint) > 0 && start(edge) != endpoint)
                || (i == 3 && sign(endpoint) < 0 && start(edge) == endpoint)
                ) ", ->-"
              else ", -<-"
            )
          }
        decoratedEdges = decoratedEdges.updated(edge, if (decoratedEdges.isDefinedAt(edge)) decoratedEdges(edge) ++ s", $styleStr" else styleStr)
      }
      // Process overcrossings if crossings are oriented
      for ((edge, endpoint) <- overcrossings if sign(endpoint) != 0) {
        val styleStr = if 
          (  (G.vertexFlags(endpoint).head._1 == edge && start(edge) == endpoint)
          || (G.vertexFlags(endpoint).head._1 != edge && G.edgeVertexIncidences(edge)._2 == endpoint)
          ) "->-" else "-<-" 
        decoratedEdges = decoratedEdges.updated(edge, if (decoratedEdges.isDefinedAt(edge)) decoratedEdges(edge) ++ s", $styleStr" else styleStr)
      }
    }

    draw(new PlanarGraph(G.outerFace, modifiedVertexFlags, G.labels, G.loops), decoratedEdges, decoratedVertices, hideBoundaryEdges, hideDiskBoundary)
  }

  private def draw(G: PlanarGraph, decoratedEdges: Map[Int, String], decoratedVertices: Map[Int, String], hideBoundaryEdges: Boolean, hideDiskBoundary: Boolean): String = {
    // Draws planar graphs, outputs LaTeX TikZ code.
    // Allows fine control of edge and internal vertex styles via decoratedEdges and decoratedVertices.
    // hideBoundary hides all vertices and edges connected to the boundary. Used mainly to draw closed graphs.
    // IMPORTANT: Doesn't draw free loops, and will not handle graphs with 1-valent internal vertices properly.
    // Will also place every 0-valent vertex at the origin.

    if (G.loops > 0) println(s"Note: graph has ${G.loops} loops which are not shown")

    def boundaryPoint(i: Int) = G.numberOfVertices + i // Boundary points are listed after the zeroth and the internal vertices.
    // So if the internal vertices are 1, ..., k, then
    // the i = 0, ..., n-1 boundary points (in clockwise order around the 0 vertex)
    // are labeled k+1, ..., k+n, in anticlockwise order around the disk boundary.

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
        edgeEndpts = (edge, if (G.edgeVertexIncidences(edge)._1 == 0) (boundaryPoint(k), target) else (target, boundaryPoint(k))) +: edgeEndpts
          // Add edge, ensuring the new label for the boundary point replaces the original 0 label. 
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
      yield (round(cos(Pi / 2 + 2 * Pi * k / G.numberOfBoundaryPoints), 6),
      round(sin(Pi / 2 + 2 * Pi * k / G.numberOfBoundaryPoints), 6))
    val internalVertexCoords =
      if (G.numberOfInternalVertices == 1) IndexedSeq((0.0, 0.0))
      else if (G.numberOfInternalVertices > 1) {
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
    def minimizingRotation(vertexCoords: (Double, Double), neighbourCoords: Seq[(Double, Double)]): Double = {
      val numberOfNeighbours = neighbourCoords.length
      val edgeOutAngles = for (i <- 0 until numberOfNeighbours) yield i * 2 * Pi / numberOfNeighbours
      val neighbourAngles = for ((x, y) <- neighbourCoords) yield {
        if (vertexCoords == (x, y)) 0
        else { val angle = atan2(y - vertexCoords._2, x - vertexCoords._1); if (angle < 0) angle + 2 * Pi else angle }
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
                        |[scale=$imageScale,${if (globalStyle != "") s"$globalStyle," else ""}
                        |->-/.style={decoration={markings, mark=at position .5 with{\\arrow{>}}}, postaction={decorate}},
                        |-<-/.style={decoration={markings, mark=at position .5 with{\\arrow{<}}}, postaction={decorate}}]
                        |${if (!hideDiskBoundary) "\\draw[gray, dashed] (0,0) circle (1.0);\n" else ""} """.stripMargin
    // Place boundary points
    for (i <- 0 until G.numberOfBoundaryPoints) {
      tikzString = tikzString ++ s"\\node${if (hideBoundaryEdges) "[draw=none, minimum size=0pt]" else ""} (${boundaryPoint(i)}) at ${boundaryPointCoords(i)} {};\n"
    }
    // Place internal vertices
    val dVs = decoratedVertices.keySet
    for (i <- 0 until G.numberOfInternalVertices) {
      tikzString = tikzString ++ s"\\node${if (dVs contains i + 1) s"[${decoratedVertices(i + 1)}]" else ""} (${i + 1}) at ${internalVertexCoords(i)} {};\n"
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
    for (t: Tuple2[Int, (Int, Int)] <- edgeEndpts) {
      val edge = t._1
      val u = t._2._1
      val v = t._2._2
      val dEs = decoratedEdges.keySet
      val edgeStr = if (u == v) { // Self-loop
        val firstEdgeIndex = cyclicReverse(G.edgesAdjacentTo(u)).indexOf(edge)
        val secondEdgeIndex = cyclicReverse(G.edgesAdjacentTo(u)).indexOf(edge, firstEdgeIndex + 1)
        assert(firstEdgeIndex != -1 && secondEdgeIndex != -1)
        val outAngle = firstEdgeIndex * 360 / G.degree(u) + vertexRotations(u - 1)
        val inAngle = secondEdgeIndex * 360 / G.degree(u) + vertexRotations(u - 1)
        s"\\draw[out=$outAngle, in=$inAngle, loop${if (dEs contains edge) s", ${decoratedEdges(edge)}" else ""}] ($u) to ($u);\n"
      } else { // Ordinary edge
        if (hideBoundaryEdges && (G.boundaryEdges contains edge)) // Hide boundary edges if required
          ""
        else
          s"\\draw[out=${getAngle(edge, u)}, in=${getAngle(edge, v)}${if (dEs contains edge) s", ${decoratedEdges(edge)}" else ""}] ($u) to ($v);\n"
      }
      tikzString = tikzString ++ edgeStr
    }

    tikzString = tikzString ++ "\\end{tikzpicture}"
    return tikzString
  }

  def pdf(pdfPath: String, g: PlanarGraph, crossings: Map[Int, Int] = Map.empty, hideDiskBoundary: Boolean = false) {
    pdfMultiple(pdfPath, Seq(g), Seq(crossings), hideDiskBoundary)
  }

  def pdfMultiple(pdfPath: String, Gs: Seq[PlanarGraph], crossings: Seq[Map[Int, Int]] = Seq.empty, hideDiskBoundary: Boolean = false): Unit = {
    // Writes TikZ to tex file and runs pdflatex
    val processedCrossings = if (crossings.isEmpty) for (i <- 0 until Gs.length) yield Map[Int, Int]() else crossings // Hacky! :\
    val outputStr = (Gs zip processedCrossings).map((t: (PlanarGraph, Map[Int, Int])) => DrawPlanarGraph(t._1, t._2, hideDiskBoundary)).mkString(
      "\\documentclass{article}\n\\usepackage{tikz}\n\\usetikzlibrary{decorations.markings}\n\\pagestyle{empty}\n\\begin{document}\n",
      "\\bigskip\\bigskip\n\n",
      "\n\\end{document}")
    
    val baseName = FilenameUtils.getBaseName(pdfPath)
    val fullPath = FilenameUtils.getFullPath(pdfPath)
    Files.write(Paths.get(s"$fullPath$baseName.tex"), (outputStr).getBytes(StandardCharsets.UTF_8))
    val pdflatexCommand = s"$pdflatexPath ${if (fullPath == "") "" else s"-output-directory=$fullPath"} $fullPath$baseName.tex"
    val pdfcropCommand = s"$pdfcropPath $fullPath$baseName.pdf $fullPath$baseName.pdf"

    try pdflatexCommand.!!
    catch {
      case e: java.lang.Exception => { println(s"Error: Problem running '$pdflatexCommand'! Maybe check the filename and that the path exists?"); throw e }
    }
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
    try pdfcropCommand.!!
    catch {
      case e: java.lang.Exception => { println(s"Error: Problem running '$pdfcropCommand'! Do you have pdfcrop installed?"); throw e }
    }
    // Clean up
    for (ext <- List("aux", "log")) {
      s"rm $fullPath$baseName.$ext".!
    }
  }
}

object DrawPlanarGraph extends DrawPlanarGraph {
  override val boundaryWeight: Double = 1.0
  override val imageScale: Double = 1.5
  override val globalStyle: String = "every node/.style={draw, circle, fill=white, inner sep=0pt, outer sep=0pt, minimum size=2.5pt}"
}

case class CustomizedDrawPlanarGraph(val boundaryWeight: Double, val imageScale: Double, val globalStyle: String) extends DrawPlanarGraph