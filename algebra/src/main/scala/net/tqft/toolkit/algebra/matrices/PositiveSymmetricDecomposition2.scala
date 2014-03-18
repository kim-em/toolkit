package net.tqft.toolkit.algebra.matrices

import scala.collection.mutable

object PositiveSymmetricDecomposition2 {

//  case class PartialSolution(M: Array[Array[Int]], A: List[Array[Int]], D: Array[Array[Int]]) {
//    def addOneColumn: Iterator[PartialSolution] = {
//      case class PartialColumn(k: Int, C: Array[Int]) {
//        def addOneEntry: Iterator[PartialColumn] = {
//          ???
//        }
//        def complete: 
//      }
//    }
//  }
//  
//  
//  def apply(M: Array[Array[Int]]): Iterator[Array[Array[Int]]] = {
//    val A = mutable.Stack[Array[Int]]()
//    val D = M.map(_.clone)
//    
//  ???  
//    
//    
////    apply(M, A = Array.fill(M.length)(Array.empty[Int]), D = M.map(_.clone))
//  }

//  def apply(M: Array[Array[Int]], A: Array[Array[Int]], D: Array[Array[Int]]): Iterator[Array[Array[Int]]] = {
//    if (D.forall(_.forall(_ == 0))) {
//      Iterator(A.map(_.clone))
//    } else {
//      apply(M, A, C = Nil, D)
//    }
//  }
//
//  
//  
//  def apply(M: Array[Array[Int]], A: Array[Array[Int]], D: Array[Array[Int]]): Iterator[Array[Array[Int]]] = {
//    if (C.size == M.length) {
//      val nA = A.zip(C.reverse).map(p => p._1 :+ p._2)
//      apply(M, nA, D)
//    } else {
//      val cs = Iterator.from(0).takeWhile(c => )
//      ???
//    }
//  }
}