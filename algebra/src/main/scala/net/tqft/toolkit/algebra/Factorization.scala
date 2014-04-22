package net.tqft.toolkit.algebra

trait Factorization[A] { 
	def factor(a: A): Map[A, Int]
	def divisors(a: A)(implicit ring: Ring[A]): Iterator[A] = {
	  factor(a).foldLeft(Iterator(ring.one))({ case (divs: Iterator[A], (b, k: Int)) => divs.flatMap(div => (0 to k).map(x => ring.multiply(div, ring.power(b, x)))) })
	}
}