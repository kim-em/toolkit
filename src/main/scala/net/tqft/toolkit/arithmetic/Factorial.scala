package net.tqft.toolkit.arithmetic
import net.tqft.toolkit.functions.Memo

object Factorial {

  def apply(n: Int) = n match {
    case 0 => 1L
    case 1 => 1L
    case n => (1L to n) reduce (_ * _)
  }
  
  val cached = Memo { apply _ }
}