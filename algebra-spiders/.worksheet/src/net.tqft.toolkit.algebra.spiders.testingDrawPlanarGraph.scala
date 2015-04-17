package net.tqft.toolkit.algebra.spiders

object testingDrawPlanarGraph {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(99); 
  val G = Plantri(5,4)(1);System.out.println("""G  : net.tqft.toolkit.algebra.spiders.PlanarGraph = """ + $show(G ));$skip(34); 
  
  val res = DrawPlanarGraph(G);System.out.println("""res  : (breeze.linalg.DenseMatrix[Double], breeze.linalg.DenseMatrix[Double], breeze.linalg.DenseVector[Double], breeze.linalg.DenseVector[Double], breeze.linalg.DenseVector[Double], breeze.linalg.DenseVector[Double]) = """ + $show(res ));$skip(34); 
  val A = res._1; val B = res._2;System.out.println("""A  : breeze.linalg.DenseMatrix[Double] = """ + $show(A ));System.out.println("""B  : breeze.linalg.DenseMatrix[Double] = """ + $show(B ));$skip(34); ;
  val x = res._3; val y = res._4;System.out.println("""x  : breeze.linalg.DenseVector[Double] = """ + $show(x ));System.out.println("""y  : breeze.linalg.DenseVector[Double] = """ + $show(y ));$skip(61); ;
  val nonperipheralXs = res._5; val nonperipheralYs = res._6;System.out.println("""nonperipheralXs  : breeze.linalg.DenseVector[Double] = """ + $show(nonperipheralXs ));System.out.println("""nonperipheralYs  : breeze.linalg.DenseVector[Double] = """ + $show(nonperipheralYs ))}
  
}
