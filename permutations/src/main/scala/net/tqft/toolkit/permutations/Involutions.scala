package net.tqft.toolkit.permutations

trait Involutions {
  type Involution = IndexedSeq[Int]

  import Ordering.Implicits._
  
  val of: Stream[List[Involution]] = Stream.cons(
    List(IndexedSeq()),
    Stream.cons(
      List(IndexedSeq(1)),
      (of.tail zip of) map (p => ((p._1 flatMap (extendInvolutionByOne _)) ::: (p._2 flatMap (extendInvolutionByTwo _))).sorted)))

  def countFixedPoints(i: Involution) = (i.zipWithIndex) count { case (a, b) => a == b + 1 }
  def countPairs(i: Involution) = (i.size - countFixedPoints(i)) / 2

  private def extendInvolutionByOne(i: Involution): List[Involution] = List(i :+ (i.size + 1))

  private def extendInvolutionByTwo(i: Involution): List[Involution] = {
    val n = i.size
    (for (j <- 1 to n + 1) yield {
      val ii = i map { m => if (m < j) m else m + 1 }
      ii.take(j - 1) ++  ((n + 2) +: ii.drop(j - 1) :+ j)
    }).toList
  }
}

object Involutions extends Involutions