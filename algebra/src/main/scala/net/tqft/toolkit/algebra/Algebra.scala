package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.modules.Module

trait Algebra[A, B] extends Ring[B] with Module[A, B]

trait AssociativeAlgebra[A, B] extends Algebra[A, B]