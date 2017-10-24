package net.tqft.toolkit.mathematica

trait MathematicaExpression[T] {
  def toInputForm(t: T): String
  def fromInputForm(t: String): T  

  def build(head: T, arguments: Seq[T]): T
}

object MathematicaExpression {
  implicit def inputFormExpression: MathematicaExpression[String] = new MathematicaExpression[String] {
    override def toInputForm(t: String) = t
    override def fromInputForm(t: String) = t
    override def build(head: String, arguments: Seq[String]) = arguments.mkString(head + "[", ", ", "]")
  }  
}