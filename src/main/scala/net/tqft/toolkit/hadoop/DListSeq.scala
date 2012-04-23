package net.tqft.toolkit.hadoop

import com.nicta.scoobi.Scoobi._
import scala.collection.SeqLike

class DListWrapper[A](dList: DList[A])  {
    import ScoobiHelper._
 
    def filter(f: A => Boolean) = new DListWrapper(dList.filter(f))
    def toList = dList.hither.toList
    def head = toList.head
    def map[B](f: A => B)(implicit manifest: Manifest[B], formatHelper: Option[WireFormat[B]] = None) = {
      implicit val wireFormat = formatHelper.get
      new DListWrapper(dList.map(f))   
    }
}
