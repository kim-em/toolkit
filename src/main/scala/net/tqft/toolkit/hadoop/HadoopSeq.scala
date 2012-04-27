package net.tqft.toolkit.hadoop

import com.nicta.scoobi.Scoobi._
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.GenericCompanion
import scala.collection.GenIterable
import scala.collection.GenSeq
import scala.collection.mutable.Builder
import net.tqft.toolkit.Logging

trait HadoopSeq[A] extends Seq[A] {
  def wireMap[B: Manifest: WireFormat](f: A => B): HadoopSeq[B]
  def invariantMap(f: A => A): HadoopSeq[A]
  def wireSortBy[B: Manifest: WireFormat: Ordering](f: A => B): HadoopSeq[A]
  def wireZip[B: Manifest: WireFormat](that: Int => B): HadoopSeq[(A, B)]
  def wireZip[B](that: HadoopSeq[B]): HadoopSeq[(A, B)]

  def manifest: Manifest[A]
  def wireFormat: WireFormat[A]
}

object HadoopSeq extends Logging { hadoopSeq =>
  private def serializingWireFormat[X]: WireFormat[X] = {
//    ???
    warn("You are using java serialization as a Hadoop wire format. This is usually not a good idea.")
    import com.nicta.scoobi.WireFormat._
    implicitly[WireFormat[Serializable]].asInstanceOf[WireFormat[X]]
  }

  private def anyRefManifest[X]: Manifest[X] = {
    manifest[AnyRef].asInstanceOf[Manifest[X]]
  }

  def from[A](elements: GenSeq[A])(implicit manifest: Manifest[A], wireFormat: WireFormat[A] = serializingWireFormat[A]): Seq[A] = {
    info("Creating new DList with " + elements.size + " elements.")
    val dList = DList(elements.seq.zipWithIndex: _*)
    new HadoopSeqImpl(dList, TagState(true, true, hint = Some(0 until elements.size toList), sizeHint = Some(elements.size)))
  }
  def apply[A](elements: A*)(implicit manifest: Manifest[A], wireFormat: WireFormat[A] = serializingWireFormat[A]): Seq[A] = from(elements)
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
  def newBuilder[A: Manifest: WireFormat] = new Builder[A, Seq[A]] {
    val listBuffer = new scala.collection.mutable.ListBuffer[A]
    override def result = HadoopSeq.from(listBuffer)
    override def clear = {
      listBuffer.clear
    }
    override def +=(a: A) = {
      listBuffer += a
      this
    }
  }

  private object HadoopSeqCompanion extends GenericCompanion[Seq] {
    override def newBuilder[A] = {
      implicit val aManifest = anyRefManifest[A]
      implicit val aWireFormat = serializingWireFormat[A]
      HadoopSeq.newBuilder[A]
    }
  }

  case class TagState(packed: Boolean, ordered: Boolean, hint: Option[List[Int]], sizeHint: Option[Int])

  object TagState {
    def apply(size: Int): TagState = TagState(true, true, Some(0 until size toList), Some(size))
  }
  
  private class HadoopSeqImpl[A](val dList: DList[(A, Int)], val tagState: TagState)(implicit val manifest: Manifest[A], val wireFormat: WireFormat[A]) extends HadoopSeq[A] {
    import ScoobiHelper._

    override def companion = HadoopSeqCompanion

    override def filter(f: A => Boolean) = new HadoopSeqImpl(dList.filter(p => f(p._1)), TagState(false, tagState.ordered, None, None))
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
      new HadoopSeqImpl(dList.filter(p => p._2 != tag), TagState(false, tagState.ordered, tagsCache.map(_.tail), sizeCache.map(_ - 1)))
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
      info("Extracting single element of HadoopSeq.")
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
          case _ => {
            info("Querying tags on HadoopSeq.")
            Some(dList.map(_._2).hither.toList)
          }
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
        info("Computing size of HadoopSeq.")
        sizeCache = Some(dList.map(_ => 1).reduce(_ + _).hither.head)
      }
    }

    override def mkString(start: String, sep: String, end: String) = start + (map(_.toString).reduce(_ + sep + _)) + end

    override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Seq[A], (A1, Int), That]): That = {
      if (tagState.packed) {
        new HadoopSeqImpl(dList.map(p => ((p._1, p._2), p._2)), TagState(true, tagState.ordered, tagsCache, sizeCache)).asInstanceOf[That]
      } else {
        val tagIndexer: Map[Int, Int] = ???
        new HadoopSeqImpl(dList.map(p => ((p._1, tagIndexer(p._2)), tagIndexer(p._2))), TagState(true, tagState.ordered, ???, Some(tags.size))).asInstanceOf[That]
      }
    }

    private def replaceTags(f: Map[Int, Int]): HadoopSeqImpl[A] = {
      val newTags = tags.map(f)
      val packed = (newTags == (0 until f.size).toList)
      val ordered = newTags == newTags.sorted
      new HadoopSeqImpl(dList.map(p => (p._1, f(p._2))), TagState(packed, ordered, Some(newTags), Some(f.size)))
    }

    private def packTags: HadoopSeqImpl[A] = {
      replaceTags(tags.zipWithIndex.toMap)
    }

    override def wireSortBy[B: Manifest: WireFormat: Ordering](f: A => B): HadoopSeq[A] = {
      val values = dList.map(p => (f(p._1), p._2)).hither.toList
      tagsCache = Some(values.map(_._2))
      val tagIndexer = values.sortBy({ p: (B, Int) => p._1 }).map(_._2).zipWithIndex.toMap
      replaceTags(tagIndexer)
    }
    override def sortBy[B](f: A => B)(implicit ord: Ordering[B]): Seq[A] = {
      implicit val bManifest = anyRefManifest[B]
      implicit val bWireFormat = serializingWireFormat[B]

      wireSortBy(f)
    }

    override def reverse = {
      replaceTags((tags zip tags.reverse).toMap)
    }

    lazy val length = {
      computeSize
      sizeCache.get
    }

    override def wireMap[B: Manifest: WireFormat](f: A => B): HadoopSeq[B] = {
      new HadoopSeqImpl(dList.map(p => (f(p._1), p._2)), tagState.copy(hint = tagsCache, sizeHint = sizeCache))
    }
    override def invariantMap(f: A => A): HadoopSeq[A] = {
      new HadoopSeqImpl(dList.map(p => (f(p._1), p._2)), tagState.copy(hint = tagsCache, sizeHint = sizeCache))
    }

    override def map[B, That](f: (A) => B)(implicit bf: CanBuildFrom[Seq[A], B, That]): That = {
      implicit val bManifest = anyRefManifest[B]
      implicit val bWireFormat = serializingWireFormat[B]

      wireMap(f).asInstanceOf[That]
    }
    override def seq = toList

    // This may well break unless A1 = A.
    override def reduce[A1 >: A](f: (A1, A1) => A1): A1 = {
      def coerced(x: A, y: A) = f(x, y).asInstanceOf[A]
      dList.map(_._1).reduce(coerced _).hither.head
    }

    override def wireZip[B: Manifest: WireFormat](s: Int => B): HadoopSeq[(A, B)] = {
      val packed = packTags
      new HadoopSeqImpl[(A, B)](packed.dList.map(p => ((p._1, s(p._2)), p._2)), packed.tagState)
    }

    override def wireZip[B](that: HadoopSeq[B]): HadoopSeq[(A, B)] = {
      ???
    }

    override def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[Seq[A], (A1, B), That]): That = {
      (if (that.isInstanceOf[HadoopSeq[_]]) {
        wireZip(that.asInstanceOf[HadoopSeq[B]])
      } else {
        val s: Int => B = that.toSeq.seq
        implicit val bManifest2 = anyRefManifest[B]
        implicit val bWireFormat = serializingWireFormat[B]

        wireZip(s)
      }).asInstanceOf[That]
    }

    // breaks unless B = A
    override def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[Seq[A], B, That]): That = {
      val patchLength = patch.size
      val patchDList = patch match {
        case patch: HadoopSeqImpl[_] => for ((a, i) <- patch.asInstanceOf[HadoopSeqImpl[A]].packTags.dList) yield { (a, i + from) }
        case _ => DList((patch.seq.asInstanceOf[Seq[A]] zip (from until from + patchLength)): _*)
      }
      val packed = packTags
      val newDList = (for (
        (a, i) <- packed.dList;
        if i < from || i >= from + replaced
      ) yield {
        if (i < from + replaced) (a, i) else (a, i + patchLength - replaced)
      }) ++ patchDList
      new HadoopSeqImpl(newDList, TagState(packed.size + patchLength - replaced)).asInstanceOf[That]
    }

  }

  // TODO override reduceLeft, reduceRight, foldLeft and foldRight as ???
}

