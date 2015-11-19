package net.tqft.toolkit.mathematica

object Mathematica {
  implicit class expressionOperations[T: MathematicaExpression](t: T) {
    private def m = implicitly[MathematicaExpression[T]]
    def evaluate(implicit kernel: MathematicaKernel) = kernel.evaluate(t)
    def apply(arguments: Seq[T]) = m.build(t, arguments)
  }

  implicit lazy val kernel = MathematicaKernel
}