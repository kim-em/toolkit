// Unfinished: enumerating planar partitions

package net.tqft.toolkit.algebra.spiders

object PlanarPartition {
  
  type Cell = Seq[Int]
  type Partition = Seq[Cell]

  private def cellsAtOrigin(vertexLabels: Seq[Int]): Seq[Cell] = {
    // Returns all possible cells in a planar partition of vertexLabels that contain the marked boundary point,
    // which should be the first entry in vertexLabels.
    assert(vertexLabels.nonEmpty) // should not be empty when we recurse
    if (vertexLabels.length == 1) {
      //println("possible cells: " + Seq[Seq[Int]]())
      Seq[Cell]()
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

  private def remainingRegions(vertexLabels: Seq[Int], cell: Cell): Seq[Cell] = {
    // Gets planar groupings of vertexLabels induced by cell
    require(vertexLabels.head == cell.head)
    (for (Seq(h, e) <- cell.sliding(2) if vertexLabels.indexOf(e) - vertexLabels.indexOf(h) > 1) yield {
      val headIndex = vertexLabels.indexOf(h)
      val endIndex = vertexLabels.indexOf(e)
      vertexLabels.slice(headIndex + 1, endIndex)
    }).toList :+ vertexLabels.slice(vertexLabels.indexOf(cell.last) + 1, vertexLabels.length)
  }
  
  /*
  def apply(vertexLabels: Seq[Int]): Seq[Partition] = {
    for every cell at the origin:
      get remaining regions
      for every partition of each remaining region:
        return [ (cell) union (all combinations of partitions of distinct regions) ]
  }
  */
}