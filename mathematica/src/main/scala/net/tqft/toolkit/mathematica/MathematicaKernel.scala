package net.tqft.toolkit.mathematica

import com.wolfram.jlink.MathLinkFactory

trait MathematicaKernel {
  lazy val link = {
    // FIXME detect directory
    // FIXME other operating systems
    // c.f. http://mathematica.stackexchange.com/a/15212
    val kernel = MathLinkFactory.createKernelLink("""-linkmode launch -linkname '"/Applications/Mathematica 9.app/Contents/MacOS/MathKernel" -mathlink'""")
    kernel.discardAnswer()
    kernel
  }

  def evaluate[T: MathematicaExpression](t: T): T = {
    def m = implicitly[MathematicaExpression[T]]
    m.fromInputForm(link.evaluateToInputForm(m.toInputForm(t), 0))
  }
}

object MathematicaKernel extends MathematicaKernel