package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.categories._

object EuclideanRings extends HomomorphismCategory[EuclideanRing] {
 type Homomorphism[A, B] = net.tqft.toolkit.algebra.categories.Homomorphism[EuclideanRing, A, B]
}