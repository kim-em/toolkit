package net.tqft.toolkit.algebra

object EuclideanRings extends HomomorphismCategory[EuclideanRing] {
 type Homomorphism[A, B] = net.tqft.toolkit.algebra.Homomorphism[EuclideanRing, A, B]
}