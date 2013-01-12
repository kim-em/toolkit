package net.tqft.toolkit.mathematica
import java.math.BigInteger
import java.io.Writer
import java.io.StringWriter

trait MathematicaExpression {
  def toMathematicaInputString: String
  def writeMathematicaInputString(writer: Writer)
}

trait ShortMathematicaExpression extends MathematicaExpression {
  final def writeMathematicaInputString(writer: Writer) = writer.write(toMathematicaInputString)
}
trait LongMathematicaExpression extends MathematicaExpression {
  final def toMathematicaInputString = {
    val w = new StringWriter
    writeMathematicaInputString(w)
    w.toString
  }
}

object MathematicaExpression {

  implicit def intToMathematicaExpression(x: Int): MathematicaExpression = new ShortMathematicaExpression {
    def toMathematicaInputString = x.toString
  }

  implicit def bigIntToMathematicaExpression(x: BigInt): MathematicaExpression = new ShortMathematicaExpression {
    def toMathematicaInputString = x.toString
  }
  implicit def bigIntegerToMathematicaExpression(x: BigInteger): MathematicaExpression = new ShortMathematicaExpression {
    def toMathematicaInputString = x.toString
  }
  implicit def doubleToMathematicaExpression(x: Double): MathematicaExpression = new ShortMathematicaExpression {
    def toMathematicaInputString = x.toString
  }
  implicit def booleanToMathematicaExpression(x: Boolean): MathematicaExpression = new ShortMathematicaExpression {
    def toMathematicaInputString = if (x) "True" else "False"
  }

  implicit def listToMathematicaExpression[A <% MathematicaExpression](x: Iterable[A]): MathematicaExpression = new LongMathematicaExpression {
    def writeMathematicaInputString(writer: Writer) = {
      writer.write("{")
      val iter = x.iterator
      if (iter.hasNext) {
        iter.next.writeMathematicaInputString(writer)
      }
      while (iter.hasNext) {
        writer.write(", ")
        iter.next.writeMathematicaInputString(writer)
      }
      writer.write("}")
    }
  }

  implicit def pairToMathematicaExpression[A <% MathematicaExpression, B <% MathematicaExpression](x: (A, B)): MathematicaExpression = new LongMathematicaExpression {
    def writeMathematicaInputString(writer: Writer) = {
      writer.write("{")
      x._1.writeMathematicaInputString(writer)
      writer.write(", ")
      x._2.writeMathematicaInputString(writer)
      writer.write("}")
    }
  }
  implicit def mapToMathematicaExpression[A <% MathematicaExpression, B <% MathematicaExpression](x: Map[A, B]): MathematicaExpression = new LongMathematicaExpression {
    def writeMathematicaInputString(writer: Writer) = {
      def writeMapEntry(m: (A, B)) = {
        m._1.writeMathematicaInputString(writer)
        writer.write(" -> ")
        m._2.writeMathematicaInputString(writer)
      }

      writer.write("{")
      for (a <- x.headOption) {
        writeMapEntry(a)
        for (a <- x.tail) {
          writer.write(", ")
          writeMapEntry(a)
        }
      }
      writer.write("}")
    }
  }
}

