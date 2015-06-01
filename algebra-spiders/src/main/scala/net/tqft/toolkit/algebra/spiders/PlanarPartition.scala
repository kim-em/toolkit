package net.tqft.toolkit.algebra.spiders

object PlanarPartition {
  def cellsAtOrigin(vertexLabels: Seq[Int]): Seq[Seq[Int]] = {
    // Returns all possible cells in a planar partition of vertexLabels that contain the marked boundary point,
    // which should be the first entry in vertexLabels.

    assert(vertexLabels.nonEmpty) // should not be empty when we recurse
    if (vertexLabels.length == 1) {
      //println("possible cells: " + Seq[Seq[Int]]())
      Seq[Seq[Int]]()
    } else if (vertexLabels.length <= 3) {
      //println("possible cells: " + Seq(vertexLabels))
      Seq(vertexLabels)
    } else {
      val n = vertexLabels.length
      (for (k <- 1 +: (3 until n)) yield {
        val e = Seq(vertexLabels.head, vertexLabels(k))
        //println(s"vertexLabels: $vertexLabels")
        //println(s"current edge: $e")
        //println(s"considering possibleCells on ${vertexLabels.head +: vertexLabels.slice(k + 1, n)}")
        val cellsInRemainder = cellsAtOrigin(vertexLabels.head +: vertexLabels.slice(k + 1, n))
        val cs = cellsInRemainder.map(e ++ _.tail)
        //println(s"combining edge $e with possibleCells above")
        //println("this stage of the recursion yields " + (if (k != n - 2) e +: cs else cs) + "\n")
        if (k != n - 2) e +: cs else cs
      }).flatten
    }
  }
  
  def remainingRegions(vertexLabels: Seq[Int], cell: Seq[Int]): Seq[Seq[Int]] = {
    // Gets planar groupings of vertexLabels induced by cell
    require(vertexLabels.head == cell.head)
    (for (Seq(h, e) <- cell.sliding(2) if vertexLabels.indexOf(e) - vertexLabels.indexOf(h) > 1) yield {
      val headIndex = vertexLabels.indexOf(h)
      val endIndex = vertexLabels.indexOf(e)
      vertexLabels.slice(headIndex + 1, endIndex)
    }).toList :+ vertexLabels.slice(vertexLabels.indexOf(cell.last) + 1, vertexLabels.length)
  }

  /*
  def cellsAtOrigin(vertexLabels: Seq[Int]): Iterator[Seq[Int]] = {
    // Returns all possible cells in a planar partition of vertexLabels that contain the marked boundary point,
    // which should be the first entry in vertexLabels.

    assert(vertexLabels.nonEmpty) // should not be empty when we recurse
    if (vertexLabels.length == 1) {
      //println("possible cells: " + Seq[Seq[Int]]())
      Iterator[Seq[Int]]()
    } else if (vertexLabels.length <= 3) {
      //println("possible cells: " + Seq(vertexLabels))
      Iterator(vertexLabels)
    } else {
      val n = vertexLabels.length
      var it = Iterator[Seq[Int]]()
      for (k <- 1 +: (3 until n)) {
        val e = Seq(vertexLabels.head, vertexLabels(k))
        //println(s"vertexLabels: $vertexLabels")
        //println(s"current edge: $e")
        //println(s"considering possibleCells on ${vertexLabels.head +: vertexLabels.slice(k + 1, n)}")
        val cellsInRemainder = cellsAtOrigin(vertexLabels.head +: vertexLabels.slice(k + 1, n))
        val cs = cellsInRemainder.map(e ++ _.tail)
        //println(s"combining edge $e with possibleCells above")
        //println("this stage of the recursion yields " + (if (k != n - 2) e +: cs else cs) + "\n")
        it = it ++ (if (k != n - 2) Iterator(e) ++ cs else cs)
      }
      it
    }
  }

  def remainingRegions(vertexLabels: Seq[Int], cell: Seq[Int]): Iterator[Seq[Int]] = {
    // Gets planar groupings of vertexLabels induced by cell
    require(vertexLabels.head == cell.head, "Cell should start at the first boundary point")
    var it = Iterator[Seq[Int]]()
    for (Seq(h, e) <- cell.sliding(2) if vertexLabels.indexOf(e) - vertexLabels.indexOf(h) > 1) {
      val headIndex = vertexLabels.indexOf(h)
      val endIndex = vertexLabels.indexOf(e)
      it = it ++ Iterator(vertexLabels.slice(headIndex + 1, endIndex))
    }

    if (vertexLabels.length - vertexLabels.indexOf(cell.last) > 1)
      it ++ Iterator(vertexLabels.slice(vertexLabels.indexOf(cell.last) + 1, vertexLabels.length))
    else it
  }
  */

  def planarPartitions(vertexLabels: Seq[Int]): Seq[Seq[Seq[Int]]] = {
    for (cell <- cellsAtOrigin(vertexLabels)) yield {
      for (region <- remainingRegions(vertexLabels, cell)) yield {
        planarPartitions(region).map( (partition: Seq[Seq[Int]]) => cell +: partition )
      }
    }
  }

}