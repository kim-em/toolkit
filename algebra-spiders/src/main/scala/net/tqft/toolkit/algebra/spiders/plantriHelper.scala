package net.tqft.toolkit.algebra.spiders

object plantriHelper {
  
  // def convert(eAdjs: IndexedSeq[IndexedSeq[Int]]) = { // debug header
  def convert(eAdjs: IndexedSeq[IndexedSeq[Int]]): PlanarGraph = {
    // Convert plantri edge code output to internal PlanarGraph representation.
    // Input:  the edge code of a graph G as a IndexedSeq of IndexedSeqs of integers.
    // The IndexedSeq of integers at position v is a CW sequence of edges out of vertex v.
    //
    // NOTE 1: In order to correctly get the outerFace parameter for PlanarGraph we require
    // the first vertex in the edge code to be on the outer face.
    // 
    // NOTE 2: Doesn't work on any graph with loops!
    
    val numOfVertices = eAdjs.length

    // Map edges (: Int) to their endpoints (: Set[Int])
    // OPTIMIZATION NOTE: Premature optimization is the root of all evil,
    // but I can't help thinking that using mutable.Set and going through eAdjs once for each vertex
    // and updating the corresponding edge set might be better.
    val vertexPairings = eAdjs.flatten.distinct. // get distinct edges
      map((e: Int) => (e, (for (v <- 0 until numOfVertices if eAdjs(v).contains(e)) yield v).toSet)).
      toMap

    // Temporary variable (: IndexedSeq[(IndexedSeq[Int], Array[Int])]) en route to constructing final vertexFlags.
    // Construct IndexedSeq of tuples; the tuple (IndexedSeq, Array) in the v-th entry describes
    // the ACW cyclic ordering of (edge, leftward face) pairings ("flags") around vertex v.
    // The IndexedSeq in the first entry stores the ACW cyclic edge adjacencies, and
    // the array (mutable!) in the second entry stores the leftward face associated to each edge.
    // We will update the arrays with face labels by traversing the graph.
    var tmpVertexFlags = eAdjs.map(L => (L.reverse, Array.fill(L.length)(0)))
    
    def traverse(vertex: Int, edgeIndex: Int, faceLabel: Int): Unit = {
      // Take a vertex and
      // *the index in tmpVertexFlags(startVertex)._1 of* an edge going out of it,
      // and traverse the boundary of the face to the left of startEdge,
      // updating the relevant entries in tmpVertexFlags with the face label as we go.
            
      def step(currentVertex: Int, outEdge: Int): (Int, Int) = {
        // Takes the current vertex we're on, and the edge to travel down,
        // and returns a tuple of (the vertex "nextVertex" we travel to, edge "leftTurn" out of nextVertex "to the left" of outEdge)
        val nextVertex = (vertexPairings(outEdge) - currentVertex).head
        // Take the next edge from outEdge in the CW sequence for nextVertex 
        val leftTurn = eAdjs(nextVertex)( (eAdjs(nextVertex).indexOf(outEdge) + 1) % eAdjs(nextVertex).length )
        
        return (nextVertex, leftTurn)
      }
      
      if ( tmpVertexFlags(vertex)._2(edgeIndex) != 0 ) // A nonzero entry indicates we've already updated the
        Unit                                           // face label for the edge out of this vertex
      else {
        tmpVertexFlags(vertex)._2(edgeIndex) = faceLabel
        val (v,e) = step(vertex, tmpVertexFlags(vertex)._1(edgeIndex))
        traverse(v, tmpVertexFlags(v)._1.indexOf(e) , faceLabel)
      }
    }
    
    // Traverse graph faces and update tmpVertexFlags
    var face = 1 // face label
    for (v <- 0 until numOfVertices) {
      var untraversedFaceAt = tmpVertexFlags(v)._2.indexOf(0) // get index of edge for which we have not yet found the left face
      while (untraversedFaceAt != -1) {
        traverse(v, untraversedFaceAt, face)
        face += 1
        untraversedFaceAt = tmpVertexFlags(v)._2.indexOf(0)
      }
    }
    
    // vertexFlags: IndexedSeq[Seq[(Int, Int)]] describes the half edges coming out of each vertex
    val vertexFlags = for (v <- 0 until numOfVertices) yield ( (for (i <- 0 until eAdjs(v).length) yield (tmpVertexFlags(v)._1(i), tmpVertexFlags(v)._2(i))).toSeq )
    val outerFace = vertexFlags(0).head._2
    val labels = 0 until numOfVertices
    val loops = 0 // We will not handle loops until I figure out how to best modify vertexPairings
                  // and traverse to accommodate edges that start and end on the same vertex.  
    
    return PlanarGraph(outerFace, vertexFlags, labels, loops)
  }
}