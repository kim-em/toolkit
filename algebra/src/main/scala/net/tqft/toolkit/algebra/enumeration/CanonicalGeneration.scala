package net.tqft.toolkit.algebra.enumeration

object CanonicalGeneration {

  def goodChildren[A, B:Ordering](children: A => Iterator[A], parents: A => Set[A], invariant: A => B): A => Iterator[A] = { a: A => 
  	for(c <- children(a); if parents(c).minBy(invariant) == a) yield c
  }  
  
}