package net.tqft.toolkit.hadoop

import com.nicta.scoobi.Scoobi._
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.GenericCompanion
import scala.collection.GenIterable
import scala.collection.GenSeq
import scala.collection.mutable.Builder
import net.tqft.toolkit.Logging

object HadoopSeq extends Logging { hadoopSeq =>
  private implicit def serializingWireFormat[X]: WireFormat[X] = {
    import com.nicta.scoobi.WireFormat._
    implicitly[WireFormat[Serializable]].asInstanceOf[WireFormat[X]]
  }

  private def anyRefManifest[X]: Manifest[X] = {
    manifest[AnyRef].asInstanceOf[Manifest[X]]
  }

  def from[A: Manifest](elements: GenSeq[A]): Seq[A] = {
    info("Creating new DList with " + elements.size + " elements.")
    val dList = DList(elements.seq.zipWithIndex: _*)
    new HadoopSeq(dList, TagState(true, true, hint = Some(0 until elements.size toList), sizeHint = Some(elements.size)))
  }
  def apply[A: Manifest](elements: A*): Seq[A] = from(elements)

  private object HadoopSeqCompanion extends GenericCompanion[Seq] {
    //    // TODO is this ridiculous? do we need to chunk?
    //    // TODO can we use futures to write the data in the background?
    //    override def newBuilder[A] = new Builder[A, Seq[A]] {
    //      implicit val manifest = anyRefManifest[A]
    //
    //      var dList = DList[(A, Int)]()
    //      var index = 0
    //      override def result = new HadoopSeq(dList, TagState(true, true, hint = Some(0 until index toList), sizeHint = Some(index)))
    //      override def clear = {
    //        dList = DList[(A, Int)]()
    //        index = 0
    //      }
    //      override def +=(a: A) = {
    //        dList = dList ++ DList((a, index))
    //        index = index + 1
    //        this
    //      }
    //    }
    override def newBuilder[A] = new Builder[A, Seq[A]] {
      implicit val manifest = anyRefManifest[A]
      var listBuffer = new scala.collection.mutable.ListBuffer[A]
      override def result = HadoopSeq.from(listBuffer)
      override def clear = {
        listBuffer.clear
      }
      override def +=(a: A) = {
        listBuffer += a
        this
      }
    }
  }

  case class TagState(packed: Boolean, ordered: Boolean, hint: Option[List[Int]], sizeHint: Option[Int])

  private class HadoopSeq[A: Manifest](val dList: DList[(A, Int)], val tagState: TagState) extends Seq[A] with SeqLike[A, Seq[A]] {
    import ScoobiHelper._

    override def companion = HadoopSeqCompanion

    override def filter(f: A => Boolean) = new HadoopSeq(dList.filter(p => f(p._1)), TagState(false, tagState.ordered, None, None))
    override def iterator = {
      info("Calling HadoopSeq.iterator.")
      if (tagState.ordered) {
        dList.map(_._1).hither.iterator
      } else {
        dList.hither.toList.sortBy(_._2).map(_._1).iterator
      }
    }
    override lazy val toList = iterator.toList

    override def head = apply(0)
    override def tail = {
      val tag = tagForIndex(0)
      new HadoopSeq(dList.filter(p => p._2 != tag), TagState(false, tagState.ordered, tagsCache.map(_.tail), sizeCache.map(_ - 1)))
    }

    private def tagForIndex(idx: Int) = {
      if (tagState.packed) {
        idx
      } else {
        tags(idx)
      }
    }

    override def apply(idx: Int) = {
      val tag = tagForIndex(idx)
      dList.filter(_._2 == tag).hither.head._1
    }

    override def isEmpty = {
      sizeCache match {
        case Some(0) => true
        case Some(_) => false
        case None => {
          tags.isEmpty
        }
      }
    }

    private var tagsCache: Option[List[Int]] = tagState.hint
    private def computeTags {
      if (tagsCache.isEmpty) {
        tagsCache = ((tagState.packed && tagState.ordered, sizeCache) match {
          case (true, Some(size)) => Some(0 until size toList)
          case _ => Some(dList.map(_._2)(manifest[Int], implicitly[WireFormat[Int]]).hither.toList)
        })
      }
    }
    private def tags = {
      computeTags
      tagsCache.get
    }
    private var sizeCache: Option[Int] = tagState.sizeHint
    private def computeSize {
      if (sizeCache.isEmpty) {
        sizeCache = Some(dList.map(_ => 1).reduce(_ + _).hither.head)
      }
    }

    override def mkString(start: String, sep: String, end: String) = start + (map(_.toString).reduce(_ + sep + _)) + end

    override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Seq[A], (A1, Int), That]): That = {
      if (tagState.packed) {
        new HadoopSeq(dList.map(p => ((p._1, p._2), p._2)), TagState(true, tagState.ordered, tagsCache, sizeCache)).asInstanceOf[That]
      } else {
        val tagIndexer: Map[Int, Int] = ???
        new HadoopSeq(dList.map(p => ((p._1, tagIndexer(p._2)), tagIndexer(p._2))), TagState(true, tagState.ordered, ???, Some(tags.size))).asInstanceOf[That]
      }
    }

    private def replaceTags(f: Map[Int, Int]): HadoopSeq[A] = {
      val newTags = tags.map(f)
      val packed = (newTags == (0 until f.size).toList)
      val ordered = newTags == newTags.sorted
      new HadoopSeq(dList.map(p => (p._1, f(p._2))), TagState(packed, ordered, Some(newTags), Some(f.size)))
    }

    private def packTags: HadoopSeq[A] = {
      replaceTags(tags.zipWithIndex.toMap)
    }

    override def sortBy[B](f: A => B)(implicit ord: Ordering[B]): Seq[A] = {
      implicit val manifest = anyRefManifest[B]

      val values = dList.map(p => (f(p._1), p._2)).hither.toList
      tagsCache = Some(values.map(_._2))
      val tagIndexer = values.sortBy({ p: (B, Int) => p._1 }).map(_._2).zipWithIndex.toMap
      replaceTags(tagIndexer)
    }

    override def reverse = {
      replaceTags((tags zip tags.reverse).toMap)
    }

    lazy val length = {
      computeSize
      sizeCache.get
    }

    override def map[B, That](f: (A) => B)(implicit bf: CanBuildFrom[Seq[A], B, That]): That = {
      implicit val manifest = anyRefManifest[B]

      new HadoopSeq(dList.map(p => (f(p._1), p._2)), tagState.copy(hint = tagsCache, sizeHint = sizeCache)).asInstanceOf[That]
    }
    override def seq = toList

    override def reduce[A1 >: A](f: (A1, A1) => A1): A1 = {
      implicit val manifest = anyRefManifest[A1]
      dList.map[A1](_._1).reduce(f).hither.head
    }

    override def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[Seq[A], (A1, B), That]): That = {
      if (that.isInstanceOf[HadoopSeq[_]]) {
        // better do it the Hadoopy way, whatever that is
        ???
      } else {
        val s: Int => B = that.toSeq.seq
        val packed = packTags
        implicit val manifest1 = anyRefManifest[A1]
        implicit val manifest2 = anyRefManifest[B]

        new HadoopSeq[(A1, B)](packed.dList.map(p => ((p._1, s(p._2)), p._2)), packed.tagState).asInstanceOf[That]
      }
    }

  }

}

