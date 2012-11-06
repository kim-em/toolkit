package net.tqft.toolkit.algebra.fusion

trait FusionBimoduleMultiplication[A] {
	def left: FusionBimodule[A]
	def right: FusionBimodule[A]
	def target: FusionBimodule[A]
}

