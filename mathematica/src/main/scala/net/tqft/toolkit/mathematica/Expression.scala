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
  abstract class SymbolSkeleton(name: String) {
    def apply(arguments: Expression *) = FullFormExpression(SymbolExpression(name), arguments)
    def unapplySeq(expression: Expression): Option[Seq[Expression]] = {
      expression match {
        case FullFormExpression(SymbolExpression(`name`), arguments) => Some(arguments)
        case _ => None
      }
    }
  }
  
  object List extends SymbolSkeleton("List")
  object Times extends SymbolSkeleton("Times")
  object Plus extends SymbolSkeleton("Plus")
  object Power extends SymbolSkeleton("Power")
}

object Expression {
  implicit def liftString(s: String): Expression = StringExpression(s)
  implicit def liftInt(i: Int): Expression = IntegerExpression(new Apint(i))
  implicit def liftSeq(s: Seq[Expression]): Expression = Symbols.List(s:_*)
  implicit def liftSeqSeq(s: Seq[Seq[Expression]]): Expression = Symbols.List(s.map(liftSeq):_*)

  implicit def liftSeqInt(s: Seq[Int]): Expression = Symbols.List(s.map(i => IntegerExpression(new Apint(i))):_*) 
  implicit def liftSeqSeqInt(s: Seq[Seq[Int]]): Expression = Symbols.List(s.map(liftSeqInt):_*)
  
  private implicit object ExpressionBuilder extends org.omath.expression.ExpressionBuilder[Expression] {
    override def createStringExpression(value: String) = StringExpression(value)
    override def createRealExpression(value: String) = RealExpression(new Apfloat(value))
    override def createIntegerExpression(value: String) = IntegerExpression(new Apint(value))
    override def createSymbolExpression(name: String) = SymbolExpression(name)
    override def createFullFormExpression(head: Expression, arguments: Seq[Expression]) = FullFormExpression(head, arguments)
  }

  implicit lazy val expression: MathematicaExpression[Expression] = new MathematicaExpression[Expression] {
    override def fromInputForm(s: String): Expression = {
      SyntaxParserImplementation.parseSyntax(s).get
    }

    override def toInputForm(e: Expression): String = {
      e match {
        case SymbolExpression(name) => name
        case StringExpression(contents) => "\"" + contents + "\""
        case IntegerExpression(value) => value.toString
        case RealExpression(value) => value.toString
        case FullFormExpression(head, arguments) => arguments.map(toInputForm).mkString(toInputForm(head) + "[", ", ", "]")
      }
    }
    override def build(head: Expression, arguments: Seq[Expression]) = FullFormExpression(head, arguments)
  }
}
  