package net.tqft.toolkit.collections

object MapTransformer {
  
  implicit class ValuesTransformable[A, B](map: scala.collection.mutable.Map[A, B]) {
    def transformValues[C](f1: B => C, f2: C => B): scala.collection.mutable.Map[A, C] =  new ValueTransformer(map, f1, f2)
    def transformSomeKeys[Z](f1: A => Option[Z], f2: Z => A): scala.collection.mutable.Map[Z, B] =  new KeyTransformer(map, f1, f2)    
    def transformKeys[Z](f1: A => Z, f2: Z => A): scala.collection.mutable.Map[Z, B] =  new KeyTransformer(map, { a: A => Some(f1(a)) }, f2)    
    def transformKeys[Z](f2: Z => A): scala.collection.mutable.Map[Z, B] =  new KeyTransformer(map, { a: A => throw new UnsupportedOperationException }, f2)    
  }

  implicit class ValuesTransformableNumeric[B](map: scala.collection.mutable.Map[String, B]) {
    def transformKeysStringToInt = map.transformKeys({ s: String => s.toInt }, { k: Int => k.toString })    
  }
  
  class ValueTransformer[A, B, C](val map: scala.collection.mutable.Map[A, B], f1: B => C, f2: C => B) extends scala.collection.mutable.Map[A, C] {
    override def get(key: A): Option[C] = {
      map.get(key) map { f1(_) }
    }

    override def contains(key: A) = map.contains(key)
    override def keys = map.keys
    override def keySet = map.keySet
    override def keysIterator = map.keysIterator
    override def size = map.size
    
    override def iterator: Iterator[(A, C)] = {
      map.iterator map { case (a, b) => (a, f1(b)) }
    }

    override def +=(kv: (A, C)) = {
      kv match {
        case (key, value) => {
          map += ((key, f2(value)))
          this
        }
      }
    }
    override def -=(key: A) = {
      map -= key
      this
    }

    override def clear = map.clear
  }
  class KeyTransformer[Z, A, B](val map: scala.collection.mutable.Map[A, B], f1: A => Option[Z], f2: Z => A) extends scala.collection.mutable.Map[Z, B] {
    override def get(key: Z): Option[B] = {
      map.get(f2(key))
    }

    override def contains(key: Z) = map.contains(f2(key))
    override def keys = map.keys flatMap { f1(_) }
    override def keySet = map.keySet flatMap { f1(_) }
    override def keysIterator = map.keysIterator flatMap { f1(_) }
    override def size = map.size
    
    override def iterator: Iterator[(Z, B)] = {
      map.iterator map { case (a, b) => (f1(a), b) } collect { case (Some(k), v) => (k, v) }
    }

    override def +=(kv: (Z, B)) = {
      kv match {
        case (key, value) => {
          map += ((f2(key), value))
          this
        }
      }
    }
    override def -=(key: Z) = {
      map -= f2(key)
      this
    }

    override def clear = map.clear
  }
}