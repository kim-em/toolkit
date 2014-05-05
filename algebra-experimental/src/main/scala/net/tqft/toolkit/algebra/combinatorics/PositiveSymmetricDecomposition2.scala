package net.tqft.toolkit.algebra.combinatorics

import net.tqft.toolkit.algebra.numberfields.NumberField
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.algebra.AlgebraicNotation
import net.tqft.toolkit.algebra.polynomials.PolynomialsOverIntegerModel
import net.tqft.toolkit.algebra.polynomials.MinimalPolynomial
import net.tqft.toolkit.algebra.Fraction

object PositiveSymmetricDecomposition2 {

  def apply[I:IntegerModel](
    m: Seq[Seq[Int]],
    numberField: NumberField[Fraction[I]],
    numericNumberFieldGenerator: Double,
    globalDimension: Polynomial[Fraction[I]],
    dimensions: Seq[Polynomial[Fraction[I]]]): Iterator[Seq[Seq[Int]]] = {
    
    import IntegerModel._
    import AlgebraicNotation._
    
    val numericGlobalDimension = globalDimension.mapValues(_.toDouble).evaluateAt(numericNumberFieldGenerator)
    val numericDimensions = dimensions.map(_.mapValues(_.toDouble).evaluateAt(numericNumberFieldGenerator))
    
    val polynomials = implicitly[PolynomialsOverIntegerModel[I]]
    
    
    val columns: Seq[List[Int]] = {
      // prepare all possible columns with dimension < globalDimension
      
      case class PartialColumn(slack: Double, dimension: Polynomial[Fraction[I]], column: List[Int], reversedDimensions: List[(Polynomial[Fraction[I]], Double)]) {
        def children: Iterator[PartialColumn] = {
          val (d, nd) = reversedDimensions.head
          for(i <- (0 until (slack / nd + 0.0001).toInt).iterator) yield {
            PartialColumn(slack - nd * i, dimension + d, i :: column, reversedDimensions.tail)
          }
        }
        def completions: Iterator[(Polynomial[Fraction[I]], List[Int])] = {
          if(reversedDimensions.nonEmpty) {
            children.flatMap(_.completions)
          } else {
            Iterator((dimension, column))
          }
        }
      }
      
      val allColumns = PartialColumn(numericGlobalDimension, polynomials.zero, Nil, dimensions.reverse.zip(numericDimensions.reverse).toList).completions
      
        import MinimalPolynomial._
      // filter out only those where the dimension doesn't divide globalDimension as an algebraic integer
      val dividingColumns = {
        allColumns.filter(c => numberField.integer_?(numberField.quotient(globalDimension, c._1)))
      }
      
      // filter out those where the dimension is not a d-number
        dividingColumns.filter(c => numberField.dNumber_?(c._1)).map(_._2).toSeq
    }
    
    // possibly, organize the columns as a tree, to speed up what follows
    
    // now, add a column at a time
    
    // only consider columns lexicographically smaller than the previous one
    // filter out some very basic inequalities (ensuring all the entries remain positive)
    // filter out by positivity, using the Cholesky decomposition
    ???
    
  }

}