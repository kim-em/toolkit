package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.categories._

trait RingHomomorphism[A, B] extends Homomorphism[Ring, A, B]

object Rings extends HomomorphismCategory[Ring]
