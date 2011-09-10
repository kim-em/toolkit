package net.tqft.toolkit.collections

object LexicographicOrdering {

  implicit def Pair2LexicographicOrderedPair[A <% Ordered[A]](pair1: (A, A)): Ordered[(A, A)] = {
    new Ordered[(A, A)] {
      def compare(pair2: (A, A)): Int = {
        val c = pair1._1 compare pair2._1
        if (c != 0) return c
        pair1._2 compare pair2._2
      }
    }
  }

  implicit def Seq2LexicographicOrderedSeq[A <% Ordered[A]](list1: Seq[A]): Ordered[Seq[A]] = {
    new Ordered[Seq[A]] {
      def compare(list2: Seq[A]): Int = {
        for ((x, y) <- list1 zip list2) {
          val c = x compare y
          if (c != 0) return c
        }
        return list1.size - list2.size
      }
    }
  }
  implicit def Set2LexicographicOrderedSet[A <% Ordered[A]](set1: Set[A]): Ordered[Set[A]] = {
    new Ordered[Set[A]] {
      def compare(set2: Set[A]): Int = {
        set1.toSeq.compare(set2.toSeq)
      }
    }
  }
  implicit def LexicographicSetOrdering[A <% Ordered[A]]: Ordering[Set[A]] = {
	  new Ordering[Set[A]] {
	 	 def compare(set1: Set[A], set2: Set[A]) = set1.compare(set2)
	  }
  }
  

  implicit def SeqSeq2LexicographicOrderedSeqSeq[A <% Ordered[A]](list1: Seq[Seq[A]]): Ordered[Seq[Seq[A]]] = Seq2LexicographicOrderedSeq(list1)
}