package net.tqft.toolkit.hadoop

import com.nicta.scoobi.Scoobi._
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

class HadoopSeq[A](dList: DList[(A, Int)]) extends SeqLike[A, HadoopSeq[A]] {
    import ScoobiHelper._
 
    implicit def serializingWireFormat[X]: WireFormat[X] = {
      import com.nicta.scoobi.WireFormat._
      implicitly[WireFormat[Serializable]].asInstanceOf[WireFormat[X]]
    }
    implicit def anyRefManifest[X]: Manifest[X] = {
      implicitly[Manifest[Any]].asInstanceOf[Manifest[X]]
    }
    
    override def filter(f: A => Boolean) = new HadoopSeq(dList.filter(p => f(p._1)))
    override def iterator = {
      dList.map(_._1).hither.iterator
    }
    override def head = apply(0)
    override def apply(idx: Int) = {
      val key = dList.map(_._2).hither.toList(idx)
      dList.filter(_._2 == key).hither.head._1
    }
    lazy val length = dList.map(_ => 1).reduce(_ + _).hither.head
    
    override def map[B, That](f: (A) => B)(implicit bf: CanBuildFrom[HadoopSeq[A], B, That]): That = {
      new HadoopSeq(dList.map(p => (f(p._1), p._2))).asInstanceOf[That]
    }
    override def newBuilder = ???
    override def seq = this
}
