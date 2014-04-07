package net.tqft.toolkit.algebra

trait Algebra[A, B] extends Ring[B] with Module[A, B]

object Algebra {
}

trait AssociativeAlgebra[A, B] extends Algebra[A, B]