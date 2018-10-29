//package net.tqft.toolkit.permutations
//
//import scala.collection.GenSeq
//import scala.language.implicitConversions
//
//object Permutations {
//  type Permutation = IndexedSeq[Int]
//
//  implicit class RichPermutation(p: Permutation) {
//    def permute[A](s: GenSeq[A]): IndexedSeq[A] = {
//      for (n <- p) yield { s(n) }
//    }
//    def inverse(): Permutation = {
//      val result = Array.fill(p.size)(0)
//      for ((n, i) <- p.zipWithIndex) {
//        result(n) = i
//      }
//      result
//    }
//  }
//
//  def identity(n: Int): Permutation = (0 to n - 1)
//  def inverse(x: Permutation): Permutation = x.zipWithIndex.sorted.map(_._2)
//
//  // return all permutations which take list1 to list2
//  def mapping[A](list1: Seq[A], list2: Seq[A]): Iterator[Permutation] = {
//    findOneMapping(list1, list2) match {
//      case Some(q) => preserving(list2) map { (p: Permutation) => p permute q }
//      case None => Iterator.empty
//    }
//  }
//  def mappingCached[A](list1: Seq[A], list2: Seq[A]): Iterator[Permutation] = {
//    findOneMapping(list1, list2) match {
//      case Some(q) => preservingCached(list2) map { (p: Permutation) => p permute q }
//      case None => Iterator.empty
//    }
//  }
//
//  def findOneMapping[A](list1: GenSeq[A], list2: GenSeq[A]): Option[Permutation] = findOneMappingWithSameTest(list1, list2)({ case (a, b) => a == b })
//
//  // TODO we only use this for relatively short lists; otherwise it should be tail-recursive
//  def findOneMappingWithSameTest[A](list1: GenSeq[A], list2: GenSeq[A])(implicit sameTest: (A, A) => Boolean): Option[Permutation] = {
//    if (list2.isEmpty) {
//      if (list1.isEmpty) {
//        Some(IndexedSeq())
//      } else {
//        None
//      }
//    } else {
//      list1.indexWhere(sameTest(_, list2.head)) match {
//        case -1 => None
//        case k => findOneMappingWithSameTest(list1.take(k) ++ list1.drop(k + 1), list2.tail) map { p => k +: (p map { x => if (x >= k) x + 1 else x }) }
//      }
//    }
//  }
//
//  def of(n: Int): Iterator[Permutation] = if (n <= 6) cachedOf(n).iterator else ofImpl(n)
//
//  def ofImpl(n: Int): Iterator[Permutation] = {
//    if (n == 0) {
//      Iterator(IndexedSeq.empty)
//    } else {
//      Iterator.iterate[Option[Permutation]](Some(identity(n)))({
//        case None => ??? // this can't happen
//        case Some(p) => {
//          val k = ((p zip p.tail) map { case (x, y) => x - y }) lastIndexWhere { _ < 0 }
//          if (k == -1) {
//            None
//          } else {
//            val l = p lastIndexWhere { _ > p(k) }
//            Some(p.take(k) ++ Seq(p(l)) ++ (p.slice(k + 1, l) ++ Seq(p(k)) ++ p.drop(l + 1)).reverse)
//          }
//        }
//      }).takeWhile(_.nonEmpty).map(_.get)
//    }
//  }
//
//  val cachedOf = IndexedSeq.tabulate(7)({ n: Int => ofImpl(n).toList })
//
//  def randomPermutationOf(n: Int): Permutation = {
//    scala.util.Random.shuffle((0 until n).toIndexedSeq)
//  }
//  def randomPermutationsOf(n: Int) = Iterator.continually(randomPermutationOf(n))
//
//  def of[A](l: List[A]): Iterator[Seq[A]] = {
//    of(l.size) map (_ permute l)
//  }
//
//  def preservingCached[A](list: Seq[A]) = {
//    val map = scala.collection.mutable.Map[A, Int]()
//    val bf = new scala.collection.mutable.ListBuffer[Int]
//    var counter = -1
//    for (a <- list) {
//      bf += (map.get(a) match {
//        case Some(k) => k
//        case None => {
//          counter = counter + 1
//          map += ((a, counter))
//          counter
//        }
//      })
//    }
//    preservingIntCached(bf.toList)
//  }
//  private val preservingIntCached = {
//    val cache = scala.collection.mutable.Map[Seq[Int], Seq[Permutation]]()
//
//    { l: List[Int] => cache.getOrElseUpdate(l, preserving(l).toList).iterator }
//  }
//
//  def preserving[A](list: Seq[A]): Iterator[Permutation] = {
//    val positions = list.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2)).values.toIndexedSeq
//    val factors = positions map { l: Seq[Int] => l.size }
//    val product = factors.reverse.foldLeft(Iterator(List[Permutation]()))((s0, s1) => s0.flatMap { a => of(s1).map(_ :: a) })
//
//    def assemblePermutations(positions: IndexedSeq[Seq[Int]], permutations: List[Permutation]): Permutation = {
//      (positions.flatten.inverse) permute ((positions zip permutations) flatMap { case (p, q) => q permute p })
//    }
//    product map { assemblePermutations(positions, _) }
//  }
//}