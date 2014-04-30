package net.tqft.toolkit.mathematica

import scala.language.implicitConversions

import org.apfloat.Apint
import org.apfloat.Apfloat
import org.omath.parser.SyntaxParserImplementation

sealed trait Expression_ extends org.omath.expression.Expression {
  def evaluate(implicit kernel: MathematicaKernel) = kernel.evaluate(this)
  def toInputForm = Expression_.expression.toInputForm(this)
}
sealed trait RawExpression extends Expression_ with org.omath.expression.RawExpression
sealed trait LiteralExpression extends RawExpression with org.omath.expression.LiteralExpression
case class SymbolExpression(name: String) extends RawExpression with org.omath.expression.SymbolExpression
case class StringExpression(contents: String) extends LiteralExpression with org.omath.expression.StringExpression
case class IntegerExpression(value: Apint) extends LiteralExpression with org.omath.expression.IntegerExpression
case class RealExpression(value: Apfloat) extends LiteralExpression with org.omath.expression.RealExpression
case class FullFormExpression(head: Expression_, arguments: Seq[Expression_]) extends Expression_ with org.omath.expression.FullFormExpression

object Symbols {
  object List {
    def apply(arguments: Expression_ *) = FullFormExpression(SymbolExpression("List"), arguments)
    def unapplySeq(expression: Expression_): Option[Seq[Expression_]] = {
      expression match {
        case FullFormExpression(SymbolExpression("List"), arguments) => Some(arguments)
        case _ => None
      }
    }
  }
  object Times {
    def apply(arguments: Expression_ *) = FullFormExpression(SymbolExpression("Times"), arguments)
  }
  object Plus {
    def apply(arguments: Expression_ *) = FullFormExpression(SymbolExpression("Plus"), arguments)
  }
  object Power {
    def apply(arguments: Expression_ *) = FullFormExpression(SymbolExpression("Power"), arguments)
  }
}

object Expression_ {
//  def apply(s: String) = expression.fromInputForm(s)

  implicit def liftString(s: String): Expression_ = StringExpression(s)
  implicit def liftInt(i: Int): Expression_ = IntegerExpression(new Apint(i))

  private implicit object ExpressionBuilder extends org.omath.expression.ExpressionBuilder[Expression_] {
    override def createStringExpression(value: String) = StringExpression(value)
    override def createRealExpression(value: String) = RealExpression(new Apfloat(value))
    override def createIntegerExpression(value: String) = IntegerExpression(new Apint(value))
    override def createSymbolExpression(name: String) = SymbolExpression(name)
    override def createFullFormExpression(head: Expression_, arguments: Seq[Expression_]) = FullFormExpression(head, arguments)
  }

  implicit lazy val expression: MathematicaExpression[Expression_] = new MathematicaExpression[Expression_] {
    override def fromInputForm(s: String): Expression_ = {
      SyntaxParserImplementation.parseSyntax(s).get
    }

    override def toInputForm(e: Expression_) = {
      e match {
        case SymbolExpression(name) => name
        case StringExpression(contents) => "\"" + contents + "\""
        case IntegerExpression(value) => value.toString
        case RealExpression(value) => value.toString
        case FullFormExpression(head, arguments) => arguments.map(toInputForm).mkString(toInputForm(head) + "[", ", ", "]")
      }
    }
    override def build(head: Expression_, arguments: Seq[Expression_]) = FullFormExpression(head, arguments)
  }
}