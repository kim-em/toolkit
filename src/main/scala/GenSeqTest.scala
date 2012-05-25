import scala.collection.GenSeq
import scala.collection.parallel.ParSeq

object GenSeqTest extends App {
  require((for (i <- (1 to 2000).par) yield {
    Thread.currentThread.getName
  }).distinct.size > 1)

  require((for (i <- (1 to 2000).par.asInstanceOf[scala.collection.GenSeq[Int]]) yield {
    Thread.currentThread.getName
  }).distinct.size > 1)

  def foo(x: GenSeq[Int]) = {
    x match {
      case x: ParSeq[_] => x.asInstanceOf[ParSeq[Int]].map(_ + 1)
      case _ => x.map(_ + 1)
    }
  }
}

