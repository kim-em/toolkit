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
    def m = implicitly[MathematicaExpression[T]]
    m.fromInputForm(link.evaluateToInputForm(m.toInputForm(t), 0))
  }
}

object MathematicaKernel extends MathematicaKernel