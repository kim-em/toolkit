package net.tqft.toolkit.algebra

object EuclideanDomains extends HomomorphismCategory[EuclideanDomain] {
 type Homomorphism[A, B] = net.tqft.toolkit.algebra.Homomorphism[EuclideanDomain, A, B]
}