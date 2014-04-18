package net.tqft.toolkit.algebra

trait Factorization[A] {
	def factor(a: A): Map[A, Int]
}