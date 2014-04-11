package net.tqft.toolkit.mathematica

import scala.language.implicitConversions


object MathematicaSyntax {

	implicit class MathematicaFunction[A, B](f: A => B) {
		def /@(s: Seq[A]): Seq[B] = s map f
		def /@(s: Set[A]): Set[B] = s map f
		def /@(s: List[A]): List[B] = s map f
		def /@(s: Pair[A, A]): Pair[B, B] = (f(s._1), f(s._2))
	}
	
}