//package net.tqft.toolkit.algebra.mathematica
//
//import scala.language.higherKinds
//
//import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
//import net.tqft.toolkit.algebra.Ring
//import net.tqft.toolkit.algebra.Fraction
//import net.tqft.toolkit.algebra.polynomials.Polynomial
//
//trait MathematicaForm[A] {
//  def toMathematicaInputString(a: A): String
//}
//
//object MathematicaForm {
//  implicit class MathematicaFormOperations[A: MathematicaForm](a: A) {
//    def toMathemathicaInputString = implicitly[MathematicaForm[A]].toMathematicaInputString(a)
//  }
//
//  implicit object StringMathematicaForm extends MathematicaForm[String] {
//    override def toMathematicaInputString(s: String) = "\"" + s + "\""
//  }
//  object BareStringMathematicaForm extends MathematicaForm[String] {
//    override def toMathematicaInputString(s: String) = s
//  }
//
//  implicit object IntMathematicaForm extends MathematicaForm[Int] {
//    override def toMathematicaInputString(i: Int) = i.toString
//  }
//  implicit object LongMathematicaForm extends MathematicaForm[Long] {
//    override def toMathematicaInputString(i: Long) = i.toString
//  }
//  implicit object BigIntMathematicaForm extends MathematicaForm[BigInt] {
//    override def toMathematicaInputString(i: BigInt) = i.toString
//  }
//
//  implicit def seqMathematicaForm[A: MathematicaForm, CC[X] <: Seq[X]] = new MathematicaForm[CC[A]] {
//    override def toMathematicaInputString(s: CC[A]) = {
//      def F = implicitly[MathematicaForm[A]]
//      s.map(x => F.toMathematicaInputString(x)).mkString("{", ", ", "}")
//    }
//  }
//  implicit def seqSeqMathematicaForm[A: MathematicaForm, CC[X] <: Seq[X]] = new MathematicaForm[CC[CC[A]]] {
//    override def toMathematicaInputString(s: CC[CC[A]]) = {
//      def F = implicitly[MathematicaForm[CC[A]]]
//      s.map(x => F.toMathematicaInputString(x)).map(" " + _).mkString("{\n", ",\n", "\n}")
//    }
//  }
//
//  def polynomialMathematicaForm[A: MathematicaForm: Ring](variableName: String) = new MathematicaForm[Polynomial[A]] {
//    override def toMathematicaInputString(p: Polynomial[A]) = {
//      def F = implicitly[MathematicaForm[A]]
//      def R = implicitly[Ring[A]]
//      if (p.coefficients.isEmpty) {
//        F.toMathematicaInputString(implicitly[Ring[A]].zero)
//      }
//      (for ((i, a) <- p.coefficients) yield {
//        val c = (if (i == 0 || a != R.one) F.toMathematicaInputString(a) else "")
//        val d = (if (i != 0) { variableName + (if (i != 1) { "^" + i.toString } else { "" }) } else { "" })
//        Seq(c, d).filter(_.nonEmpty).mkString(" * ")
//      }).mkString(" + ")
//    }
//  }
//
//  implicit def fractionMathematicaForm[A: MathematicaForm: Ring] = new MathematicaForm[Fraction[A]] {
//    override def toMathematicaInputString(f: Fraction[A]) = {
//      implicit def F = implicitly[MathematicaForm[A]]
//
//      if (f.denominator == implicitly[Ring[A]].one) {
//        F.toMathematicaInputString(f.numerator)
//      } else {
//        "(" + F.toMathematicaInputString(f.numerator) + ") / (" + F.toMathematicaInputString(f.denominator) + ")"
//      }
//    }
//  }
//
//  implicit def multivariablePolynomialMathematicaForm[A: MathematicaForm]: MathematicaForm[MultivariablePolynomial[A, String]] = multivariablePolynomialMathematicaForm(implicitly[MathematicaForm[A]], BareStringMathematicaForm)
//
//  implicit def multivariablePolynomialMathematicaForm[A: MathematicaForm, V: MathematicaForm]: MathematicaForm[MultivariablePolynomial[A, V]] = new MathematicaForm[MultivariablePolynomial[A, V]] {
//    override def toMathematicaInputString(p: MultivariablePolynomial[A, V]) = {
//      if (p.coefficients.isEmpty) {
//        "0"
//      } else {
//        p.coefficients.toSeq.map({
//          case (m, a) => {
//            val showCoefficient = m.isEmpty || a.toMathemathicaInputString != "1"
//            val showMonomial = m.nonEmpty
//            (if (showCoefficient) "(" + a.toMathemathicaInputString + ")" else "") + (if (showCoefficient && showMonomial) " * " else "") + (if (showMonomial) { m.map({ case (v, k) => v.toMathemathicaInputString + (if (k > 1) "^" + k else "") }).mkString(" * ") } else "")
//          }
//        }).mkString(" + ")
//      }
//    }
//  }
//}