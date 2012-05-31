package net.tqft.toolkit.permutations

object Involutions {
  type Involution = List[Int]

  import net.tqft.toolkit.collections.LexicographicOrdering._

  val of: Stream[List[Involution]] = Stream.cons(
    List(List()),
    Stream.cons(
      List(List(1)),
      (of.tail zip of) map (p => ((p._1 flatMap (extendInvolutionByOne _)) ::: (p._2 flatMap (extendInvolutionByTwo _))) sortWith { _ < _ })))

  def countFixedPoints(i: Involution) = (i zipWithIndex) count { case (a, b) => a == b + 1 }
  def countPairs(i: Involution) = (i.size - countFixedPoints(i))/2
  
  private def extendInvolutionByOne(i: Involution): List[Involution] = List(i ::: List(i.size + 1))

  private def extendInvolutionByTwo(i: Involution): List[Involution] = {
    val n = i.size
    (for (j <- 1 to n + 1) yield {
      val ii = i map { m => if (m < j) m else m + 1 }
      ii.take(j - 1) ::: List(n + 2) ::: ii.drop(j - 1) ::: List(j)
    }).toList
  }
}