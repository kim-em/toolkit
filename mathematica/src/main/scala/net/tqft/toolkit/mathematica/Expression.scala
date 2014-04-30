package net.tqft.toolkit.mathematica

import scala.language.implicitConversions 

import org.apfloat.Apint
import org.apfloat.Apfloat
import org.omath.parser.SyntaxParserImplementation

sealed trait Expression extends org.omath.expression.Expression {
  def evaluate(implicit kernel: MathematicaKernel) = kernel.evaluate(this)
  def toInputForm = Expression.expression.toInputForm(this)
}
sealed trait RawExpression extends Expression with org.omath.expression.RawExpression
sealed trait LiteralExpression extends RawExpression with org.omath.expression.LiteralExpression
case class SymbolExpression(name: String) extends RawExpression with org.omath.expression.SymbolExpression
case class StringExpression(contents: String) extends LiteralExpression with org.omath.expression.StringExpression
case class IntegerExpression(value: Apint) extends LiteralExpression with org.omath.expression.IntegerExpression
case class RealExpression(value: Apfloat) extends LiteralExpression with org.omath.expression.RealExpression
case class FullFormExpression(head: Expression, arguments: Seq[Expression]) extends Expression with org.omath.expression.FullFormExpression

object Symbols {
  object List {
    def apply(arguments: Expression*) = FullFormExpression(SymbolExpression("List"), arguments)
    def unapplySeq(expression: Expression): Option[Seq[Expression]] = {
      expression match {
        case FullFormExpression(SymbolExpression("List"), arguments) => Some(arguments)
        case _ => None
      }
    }
  }
  object Times {
    def apply(arguments: Expression*) = FullFormExpression(SymbolExpression("Times"), arguments)
  }
  object Plus {
    def apply(arguments: Expression*) = FullFormExpression(SymbolExpression("Plus"), arguments)
  }
  object Power {
    def apply(arguments: Expression*) = FullFormExpression(SymbolExpression("Power"), arguments)
  }
}

object Expression {
  def apply(s: String) = expression.fromInputForm(s)
  
  implicit def liftString(s: String): Expression = StringExpression(s)
  implicit def liftInt(i: Int): Expression = IntegerExpression(new Apint(i))

  implicit def expression: MathematicaExpression[Expression] = new MathematicaExpression[Expression] {
    override def toInputForm(e: Expression) = {
      e match {
        case SymbolExpression(name) => name
        case StringExpression(contents) => "\"" + contents + "\""
        case IntegerExpression(value) => value.toString
        case RealExpression(value) => value.toString
        case FullFormExpression(head, arguments) => arguments.map(toInputForm).mkString(toInputForm(head) + "[", ", ", "]")
      }
    }
    implicit object ExpressionBuilder extends org.omath.expression.ExpressionBuilder[Expression] {
      override def createStringExpression(value: String) = StringExpression(value)
      override def createRealExpression(value: String) = RealExpression(new Apfloat(value))
      override def createIntegerExpression(value: String) = IntegerExpression(new Apint(value))
      override def createSymbolExpression(name: String) = SymbolExpression(name)
      override def createFullFormExpression(head: Expression, arguments: Seq[Expression]) = FullFormExpression(head, arguments)
    }
    override def fromInputForm(s: String) = {
      SyntaxParserImplementation.parseSyntax(s).get
    }
    override def build(head: Expression, arguments: Seq[Expression]) = FullFormExpression(head, arguments)
  }
}