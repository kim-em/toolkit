package net.tqft.toolkit
import java.math.BigInteger

trait MathematicaExpression {
  def toMathematicaInputString: String
}

object MathematicaExpression {
  implicit def intToMathematicaExpression(x: Int) = new MathematicaExpression {
    def toMathematicaInputString = x.toString
  }

  implicit def bigIntToMathematicaExpression(x: BigInt) = new MathematicaExpression {
    def toMathematicaInputString = x.toString
  }
  implicit def bigIntegerToMathematicaExpression(x: BigInteger) = new MathematicaExpression {
    def toMathematicaInputString = x.toString
  }
  implicit def doubleToMathematicaExpression(x: Double) = new MathematicaExpression {
    def toMathematicaInputString = x.toString
  }

  implicit def listToMathematicaExpression[A <% MathematicaExpression](x: Iterable[A]): MathematicaExpression = new MathematicaExpression {
    def toMathematicaInputString = {
      (x map { _.toMathematicaInputString }) mkString ("{", ", ", "}")
    }
  }

  implicit def pairToMathematicaExpression[A <% MathematicaExpression, B <% MathematicaExpression](x: (A, B)): MathematicaExpression = new MathematicaExpression {
    def toMathematicaInputString = {
      "{" + x._1.toMathematicaInputString + ", " + x._2.toMathematicaInputString + "}"
    }

  }
  implicit def mapToMathematicaExpression[A <% MathematicaExpression, B <% MathematicaExpression](x: Map[A, B]): MathematicaExpression = new MathematicaExpression {
    def toMathematicaInputString = {
      (x map { case (a, b) => a.toMathematicaInputString + " -> " + b.toMathematicaInputString }) mkString ("{", ", ", "}")
    }
  }
}

