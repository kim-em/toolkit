//package net.tqft.toolkit.amazon
//
//import com.xerox.amazonws.simpledb._
//import scala.collection.mutable
//import scala.collection.JavaConversions
//import net.tqft.toolkit.Logging
//
//trait SimpleDB extends mutable.Map[String, mutable.Map[String, mutable.Map[String, mutable.Set[String]]]] with Logging {
//  def AWSAccount: String
//  def AWSSecretKey: String
//
//  lazy val SimpleDBService = new com.xerox.amazonws.simpledb.SimpleDB(AWSAccount, AWSSecretKey, true)
//
//  override def -=(domain: String): this.type = {
//    try {
//      SimpleDBService.deleteDomain(domain)
//    } catch {
//      case e: Exception =>
//        Logging.error("exception while deleting domain " + domain + " in SimpleDB.", e)
//    }
//    this
//  }
//
//  override def +=(kv: (String, mutable.Map[String, mutable.Map[String, mutable.Set[String]]])): this.type = {
//    require(kv._2.isEmpty)
//    try {
//      SimpleDBService.createDomain(kv._1)
//    } catch {
//      case e: Exception =>
//        Logging.error("exception while creating domain " + kv._1 + " in SimpleDB.", e)
//    }
//    this
//  }
//
//  override def iterator = {
//    import JavaConversions._
//    try {
//      SimpleDBService.listDomains().getItems().map(d => (d.getName, new Domain(d))).iterator
//    } catch {
//      case e: Exception =>
//        Logging.error("exception while listing domains in SimpleDB.", e)
//        Iterator.empty
//    }
//  }
//
//  override def get(domain: String): Option[Domain] = {
//    Some(new Domain(SimpleDBService.getDomain(domain)))
//  }
//
//  class Domain(domain: com.xerox.amazonws.simpledb.Domain) extends mutable.Map[String, mutable.Map[String, mutable.Set[String]]] {
//    override def -=(item: String): this.type = {
//      try {
//        domain.deleteItem(item)
//      } catch {
//        case e: Exception =>
//          Logging.error("exception while deleting item " + item + " in SimpleDB.", e)
//      }
//      this
//    }
//
//    override def +=(kv: (String, mutable.Map[String, mutable.Set[String]])): this.type = {
//      import JavaConversions._
//      try {
//        domain.addItem(kv._1, kv._2.mapValues(mutableSetAsJavaSet(_)), Nil)
//      } catch {
//        case e: Exception =>
//          Logging.error("exception while adding item " + kv + " in SimpleDB.", e)
//      }
//      this
//    }
//
//    override def iterator = {
//     ???  
//    }
//    
//    override def get(item: String): Option[Item] = {
//      Some(new Item(???))
//    }
//    
//    class Item(name: String) extends mutable.Map[String, mutable.Set[String]] {
//      override def -=(attribute: String): this.type = {
//        ???
//        this
//      }
//      override def +=(kv: (String, mutable.Set[String])): this.type = {
//        ???
//        this
//      }
//      override def iterator = ???
//      override def get(attribute: String): Option[mutable.Set[String]] = ???
//    }
//  }
//}
//
//object SimpleDB extends SimpleDB {
//  val AWSAccount = ???
//  val AWSSecretKey = ???
//}