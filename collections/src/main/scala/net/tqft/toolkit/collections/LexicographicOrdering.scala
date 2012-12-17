package net.tqft.toolkit.collections

object LexicographicOrdering {

  implicit def Pair2LexicographicOrderedPair[A <% Ordered[A], B <% Ordered[B]](pair1: (A, B)): Ordered[(A, B)] = {
    new Ordered[(A, B)] {
      def compare(pair2: (A, B)): Int = {
        val c = pair1._1 compare pair2._1
        if (c != 0) return c
        pair1._2 compare pair2._2
      }
    }
  }
  implicit def LexicographicPairOrdering[A <% Ordered[A], B <% Ordered[B]]: Ordering[(A, B)] = {
    new Ordering[(A, B)] {
      def compare(pair1: (A, B), pair2: (A, B)) = pair1.compare(pair2)
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
  implicit def LexicographicIndexedSeqOrdering[A <% Ordered[A]]: Ordering[IndexedSeq[A]] = {
    new Ordering[IndexedSeq[A]] {
      def compare(seq1: IndexedSeq[A], seq2: IndexedSeq[A]) = seq1.compare(seq2)
    }
  }
  implicit def LexicographicSeqOrdering[A <% Ordered[A]]: Ordering[Seq[A]] = {
    new Ordering[Seq[A]] {
      def compare(seq1: Seq[A], seq2: Seq[A]) = seq1.compare(seq2)
    }
  }
  implicit def Option2LexicographicOrderedOption[A <% Ordered[A]](o1: Option[A]): Ordered[Option[A]] = {
    new Ordered[Option[A]] {
      def compare(o2: Option[A]): Int = {
    	(o1, o2) match {
    	  case (None, None) => 0
    	  case (None, Some(_)) => -1
    	  case (Some(_), None) => 1
    	  case (Some(x1), Some(x2)) => x1 compare x2
    	}
      }
    }
  }
  implicit def LexicographicOptionOrdering[A <% Ordered[A]]: Ordering[Option[A]] = {
    new Ordering[Option[A]] {
      def compare(o1: Option[A], o2: Option[A]) = o1.compare(o2)
    }
  }
  implicit def Map2LexicographicOrderedMap[A <% Ordered[A], B <% Ordered[B]](map1: Map[A, B]): Ordered[Map[A, B]] = {
    new Ordered[Map[A, B]] {
      def compare(map2: Map[A, B]): Int = {
        for (k <- (map1.keySet ++ map2.keySet).toList.sorted) {
          val c = map1.get(k) compare map2.get(k)
          if (c != 0) return c
        }
        return 0
      }
    }
  }
  implicit def LexicographicMapOrdering[A <% Ordered[A], B <% Ordered[B]]: Ordering[Map[A, B]] = {
    new Ordering[Map[A, B]] {
      def compare(map1: Map[A, B], map2: Map[A, B]) = map1.compare(map2)
    }
  }
  implicit def LexicographicListOrdering[A <% Ordered[A]]: Ordering[List[A]] = {
    new Ordering[List[A]] {
      def compare(list1: List[A], list2: List[A]) = list1.compare(list2)
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
  
  implicit def LexicographicSeqSeqOrdering[A <% Ordered[A]]: Ordering[Seq[Seq[A]]] = new Ordering[Seq[Seq[A]]] {
    def compare(seq1: Seq[Seq[A]], seq2: Seq[Seq[A]]) = seq1.compare(seq2)
  }
  implicit def LexicographicListListOrdering[A:Ordering]: Ordering[List[List[A]]] = new Ordering[List[List[A]]] {
    def compare(seq1: List[List[A]], seq2: List[List[A]]) = seq1.compare(seq2)
  }
}