package net.tqft.toolkit.hadoop

import com.nicta.scoobi.Scoobi._
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.GenericCompanion
import scala.collection.GenIterable
import scala.collection.GenSeq
import scala.collection.mutable.Builder
import net.tqft.toolkit.Logging

object FakeManifest {
  def apply() = manifest[AnyRef]
}

object HadoopSeq extends Logging { hadoopSeq =>
  private implicit def serializingWireFormat[X]: WireFormat[X] = {
    import com.nicta.scoobi.WireFormat._
    implicitly[WireFormat[Serializable]].asInstanceOf[WireFormat[X]]
  }
  private implicit def anyRefManifest[X]: Manifest[X] = {
    FakeManifest().asInstanceOf[Manifest[X]]
  }

  def from[A](elements: GenSeq[A]): Seq[A] = {
    info("Creating new DList with " + elements.size + " elements.")
//    println(elements)
    val dList = DList(elements.seq.zipWithIndex: _*)
    info("... finished creating DList.")
    new HadoopSeq(dList, true, sizeHint = Some(elements.size))
  }
  def apply[A](elements: A*): Seq[A] = from(elements)

  private object HadoopSeqCompanion extends GenericCompanion[Seq] {
//    override def newBuilder[A] = new Builder[A, Seq[A]] {
//      val lb = new scala.collection.mutable.ListBuffer[A]
//      def result = hadoopSeq.from(lb.result)
//      def clear = lb.clear
//      def +=(a: A) = {
//        lb += a
//        this
//      }
//    }
    // TODO is this ridiculous? do we need to chunk?
    // TODO can we use futures to write the data in the background?
    override def newBuilder[A] = new Builder[A, Seq[A]] {
      var dList = DList[(A, Int)]()
      var index = 0
      override def result = new HadoopSeq(dList, true, sizeHint = Some(index))
      override def clear = {
        dList = DList[(A, Int)]()
        index = 0
      }
      override def +=(a: A) = {
        dList = dList ++ DList((a, index))
        index = index + 1
        this
      }
    }
  }

  // tags are horribly broken. if any of this works, it's not my fault.
  private class HadoopSeq[A](dList: DList[(A, Int)], tagIsIndex: Boolean, sizeHint: Option[Int]) extends Seq[A] with SeqLike[A, Seq[A]] {
    import ScoobiHelper._

    override def companion = HadoopSeqCompanion

    override def filter(f: A => Boolean) = new HadoopSeq(dList.filter(p => f(p._1)), false, None)
    override def iterator = {
      dList.map(_._1).hither.iterator
    }
    override def head = apply(0)
    
    override def apply(idx: Int) = {
      val tag = if(tagIsIndex) {
        idx
      } else {
        tags(idx)
      }
      dList.filter(_._2 == tag).hither.head._1
    }
    override def isEmpty = {
      nextSizeHint match {
        case Some(0) => true
        case Some(_) => false
        case None => {
          tags.isEmpty
        }
      }
    }
    
    
    private var tagsCache: Option[List[Int]] = None
    private def computeTags {
      if (tagsCache.isEmpty) {
        tagsCache = ((tagIsIndex, nextSizeHint) match {
          case (true, Some(size)) => Some(0 until size toList)
          case _ => Some(dList.map(_._2).hither.toList)
        })
      }
    }
    private def tags = {
      computeTags
      tagsCache.get
    }

    override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Seq[A], (A1, Int), That]): That = {
      if (tagIsIndex) {
        new HadoopSeq(dList.map(p => ((p._1, p._2), p._2)), true, sizeHint = nextSizeHint).asInstanceOf[That]
      } else {
        val tagIndexer = tags.zipWithIndex.toMap
        new HadoopSeq(dList.map(p => ((p._1, tagIndexer(p._2)), tagIndexer(p._2))), true, sizeHint = Some(tagIndexer.size)).asInstanceOf[That]
      }
    }

    private def replaceTags(f: Map[Int, Int], tagsPacked: Boolean): HadoopSeq[A] = {
      new HadoopSeq(dList.map(p => (p._1, f(p._2))), tagsPacked, Some(f.size))      
    }
    
    override def sortBy[B](f: A => B)(implicit ord: Ordering[B]): Seq[A] = {
      val tagIndexer = dList.map(p => (f(p._1), p._2)).hither.toList.sortBy({ p: (B, Int) => p._1 }).map(_._2).zipWithIndex.toMap
      replaceTags(tagIndexer, true)
    }

    override def reverse = {
      replaceTags((tags zip tags.reverse).toMap, tagIsIndex)
    }
    
    private var lengthComputed: Option[Int] = None
    private def computeLength {
      if (sizeHint.nonEmpty) {
        lengthComputed = sizeHint
      } else {
        if (lengthComputed.isEmpty) {
          lengthComputed = Some(dList.map(_ => 1).reduce(_ + _).hither.head)
        }
      }
    }
    private def nextSizeHint = sizeHint.orElse(lengthComputed)

    lazy val length = {
      if (lengthComputed.isEmpty) computeLength
      lengthComputed.get
    }

    override def map[B, That](f: (A) => B)(implicit bf: CanBuildFrom[Seq[A], B, That]): That = {
      new HadoopSeq(dList.map(p => (f(p._1), p._2)), tagIsIndex, nextSizeHint).asInstanceOf[That]
    }
    override def seq = this

//    override def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[Seq[A], (A1, B), That]): That = {
//      toList.zip(that)
//    }
  }

}

