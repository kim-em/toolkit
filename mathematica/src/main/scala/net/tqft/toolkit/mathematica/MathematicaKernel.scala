package net.tqft.toolkit.mathematica

import com.wolfram.jlink.MathLinkFactory
import java.io.File

trait MathematicaKernel {
  lazy val link = {
    // FIXME other operating systems
    // c.f. http://mathematica.stackexchange.com/a/15212

    val mathematicaDirectory = new File("/Applications/").listFiles().filter(_.getName.contains("Mathematica")).sorted.last

    val kernel = MathLinkFactory.createKernelLink(s"""-linkmode launch -linkname '"$mathematicaDirectory/Contents/MacOS/MathKernel" -mathlink'""")
    kernel.discardAnswer()
    kernel
  }

  def evaluate[T: MathematicaExpression](t: T): T = {
    synchronized {
      val m = implicitly[MathematicaExpression[T]]
      val input = m.toInputForm(t)
      val evaluation = link.evaluateToInputForm(input, 0)
      m.fromInputForm(evaluation)
    }
  }
}

object MathematicaKernel extends MathematicaKernel